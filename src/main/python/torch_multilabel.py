import os
import math
import torch
import transformers
from typing import Tuple, List
import numpy as np
import anafora, anafora.evaluate 
import spacy
from transformers import PreTrainedTokenizerFast,AutoTokenizer
from sklearn.preprocessing import MultiLabelBinarizer
from torch.utils.data import Dataset, DataLoader
import logging
from transformers import RobertaModel, RobertaForTokenClassification,RobertaForTokenClassification, Trainer, TrainingArguments
import torch.nn as nn
from torch.nn import BCEWithLogitsLoss
import argparse
from typing import List, Optional, Tuple, Union
from transformers.modeling_outputs import TokenClassifierOutput
import logging
from timenorm_data_provider import TimeDataProvider

#GPU set up 
from torch import cuda
device = 'cuda' if cuda.is_available() else 'cpu'

#load necessary data
types = []
with open('types.txt','r') as f:
    lines = f.readlines()
for line in lines:
    types.append(line.replace("\n", ""))

#custom dataset 
class MultiLabelDataset(Dataset):

    def __init__(self, encodings, labels):
      self.encodings = encodings
      self.labels = labels

    def __getitem__(self, idx):
        item = {key: val[idx] for key, val in self.encodings.items()}
        item['labels'] = self.labels[idx]
        return item

    def __len__(self):
        return len(self.labels)

def calculate_pos_weights(train_labels):
    class_weights = {idx:0 for idx,_ in enumerate(types)}
    
    for sent_label in train_labels:
        for tok_label in sent_label:
            label =np.argmax(tok_label).item()
            class_weights[label]+=1
            
    total = sum(list(class_weights.values()))
    del class_weights[0] #exclude None label class weight
    
    for key, value in class_weights.items():
        if value!=0:
            class_weights[key] = math.log(total-value)/(value+1e-5) #negative examples/positive examples
        else:
            class_weights[key]=1
    return torch.as_tensor(list(class_weights),dtype=torch.float).to(device='cuda')

def _can_merge(text, entity, label, start):
    if entity is None:
        return False
    if label != entity.type:
        return False
    (_, last_end), = entity.spans
    intervening_text = text[last_end:start]
    return not intervening_text or intervening_text.isspace()

def iter_anafora(anafora_root_dir):
    for sub_dir, text_file_name, [xml_file_name] in anafora.walk(
            anafora_root_dir, "TimeNorm[.]gold[.]completed[.]xml$"):
        text_path = os.path.join(anafora_root_dir, sub_dir, text_file_name)
        with open(text_path) as text_file:
            text = text_file.read()
        yield sub_dir, text_file_name, xml_file_name, text.rstrip()
        
def iter_tokens( inputs, sentences):
    for sent_index, sentence in enumerate(sentences):
        token_tuples = []
        for token_index in range(inputs["input_ids"].shape[1]):
            offsets = inputs["offset_mapping"][sent_index][token_index].numpy()
            start, end = [sentence.start_char + o for o in offsets]
            token_id = inputs["input_ids"][sent_index][token_index]
            token_tuples.append((token_index, token_id, start, end))
        yield sent_index, sentence, token_tuples


#overriding token classification class for multi label token classification 
class RobertaForMultiTokenClassification(RobertaForTokenClassification):

    def __init__(self, config, pos_weight):
        super().__init__(config)
        self.num_labels = config.num_labels
        self.max_length = config.max_length 
        self.pos_weight = pos_weight

    def forward(
        self,
        input_ids: Optional[torch.LongTensor] = None,
        attention_mask: Optional[torch.FloatTensor] = None,
        token_type_ids: Optional[torch.LongTensor] = None,
        position_ids: Optional[torch.LongTensor] = None,
        head_mask: Optional[torch.FloatTensor] = None,
        inputs_embeds: Optional[torch.FloatTensor] = None,
        labels: Optional[torch.LongTensor] = None,
        output_attentions: Optional[bool] = None,
        output_hidden_states: Optional[bool] = None,
        return_dict: Optional[bool] = None,
    ) -> Union[Tuple, TokenClassifierOutput]:
        r"""
        labels (`torch.LongTensor` of shape `(batch_size, sequence_length)`, *optional*):
            Labels for computing the token classification loss. Indices should be in `[0, ..., config.num_labels - 1]`.
        """
        return_dict = return_dict if return_dict is not None else self.config.use_return_dict

        outputs = self.roberta(
            input_ids,
            attention_mask=attention_mask,
            token_type_ids=token_type_ids,
            position_ids=position_ids,
            head_mask=head_mask,
            inputs_embeds=inputs_embeds,
            output_attentions=output_attentions,
            output_hidden_states=output_hidden_states,
            return_dict=return_dict,
        )

        sequence_output = outputs[0]

        sequence_output = self.dropout(sequence_output)
        logits = self.classifier(sequence_output)

        loss = None
        
        if labels is not None:
            loss_fct = BCEWithLogitsLoss( reduction='none',  pos_weight=self.pos_weight ) #apply pos_weight to handle class imbalance
            loss = loss_fct(logits.view(-1, self.num_labels), labels.view(-1,self.num_labels))
            #mask padding tokens 
            loss = torch.reshape(loss.unsqueeze(0), (-1,self.max_length,self.num_labels))
            loss = torch.mean(torch.mul(loss,attention_mask.unsqueeze(-1)))

        if not return_dict:
            output = (logits,) + outputs[2:]
            return ((loss,) + output) if loss is not None else output

        return TokenClassifierOutput(
            loss=loss,
            logits=logits,
            hidden_states=outputs.hidden_states,
            attentions=outputs.attentions,
        )

def train(tokenizer, num_epochs, train_batch_size, eval_batch_size,lr_rate, data_dir):
    train_dir = os.path.join(data_dir,"train")
    dev_dir = os.path.join(data_dir,"dev")

    train_data_loader = TimeDataProvider(corpus_dir=train_dir)
    val_data_loader = TimeDataProvider(corpus_dir=dev_dir)

    #prepare trainig/val dataset
    train_inputs, train_labels = train_data_loader.read_data_to_multi_label_format(tokenizer, types=types,max_length=128)
    train_dataset = MultiLabelDataset(train_inputs, train_labels)

    val_inputs, val_labels =val_data_loader.read_data_to_multi_label_format(tokenizer, types=types,max_length=128)
    val_dataset = MultiLabelDataset(val_inputs, val_labels)

    training_args = TrainingArguments(
        output_dir='./results',          # output directory
        num_train_epochs=num_epochs,              # total number of training epochs
        per_device_train_batch_size=train_batch_size,  # batch size per device during training
        per_device_eval_batch_size=eval_batch_size,   # batch size for evaluation
        learning_rate = lr_rate,
        warmup_steps=500,                # number of warmup steps for learning rate scheduler
        weight_decay=0.01, 
        load_best_model_at_end=True,            
        logging_dir='./logs',            # directory for storing logs
        logging_steps=10,
        evaluation_strategy = "epoch", #To calculate metrics per epoch
        save_strategy = "epoch"
    )
    #load our model
    pos_weight = calculate_pos_weights(train_labels)  
    model =  RobertaForMultiTokenClassification.from_pretrained('roberta-base', num_labels=len(types)-1, max_length=128, pos_weight = pos_weight)

    #training using trainer 
    trainer = Trainer(
        model=model,                         
        args=training_args,                 
        train_dataset=train_dataset,        
        eval_dataset=val_dataset             
    )

    trainer.train()
    
    return trainer

def evaluate(tokenizer, trainer, reference_dir, output_dir):
    nlp = spacy.load("en_core_web_lg")
    for sub_dir, text_file_name, xml_file_name, text in iter_anafora(reference_dir):
        sentences = [sentence for sentence in nlp(text).sents]

        #write the prediction of the test set
        test_provider = TimeDataProvider(corpus_dir=os.path.join(reference_dir,sub_dir))
        test_inputs, test_labels = test_provider.read_data_to_multi_label_format(tokenizer, types=types,max_length=128)
        test_dataset = MultiLabelDataset(test_inputs, test_labels)
        logits = trainer.predict(test_dataset)[0] #get the logits 
        labels = np.where(torch.sigmoid(torch.from_numpy(logits))>0.5 , 1, 0)
        data = anafora.AnaforaData()
        n = 1
        entity = None
        #iterate through the sentences of this document 
        test_inputs_w_offsets = test_provider.read_data_to_multi_label_format(tokenizer, types=types,max_length=128,eval=True)[0]
        for i, sentence, token_tuples in iter_tokens(inputs=test_inputs_w_offsets, sentences=sentences):
            for j, token_id, start, end in token_tuples:
                if token_id not in tokenizer.all_special_ids:
                    possible_labels = np.where(labels[i][j]==1)[0] #get the possible labels of this token 
                    #create entity for all possible labels
                    for p_label in possible_labels:
                        label = types[p_label]
                        if label is not "None":
                            print(f'{start}:{end} {label} {text[start:end]!r}')
                            if _can_merge(text, entity, label, start):
                                (start, _), = entity.spans
                            else:
                                entity = anafora.AnaforaEntity()
                                entity.id = f"{n}@e@{text_file_name}@system"
                                entity.type = label
                                data.annotations.append(entity)
                                n += 1
                            entity.spans = (start, end),

        output_sub_dir = os.path.join(output_dir, sub_dir)
        if not os.path.exists(output_sub_dir):
            os.makedirs(output_sub_dir, exist_ok=True)
        #write to anafora file 
        xml_file_name = xml_file_name.replace("gold", "system")
        data.to_file(os.path.join(output_sub_dir, xml_file_name))

    #get the evaluation scores 
    file_scores =anafora.evaluate.score_dirs(reference_dir, output_dir,include={("*", "<span>")}, exclude={("Event", "<span>"), ("NotNormalizable", "<span>"),("Modifier", "<span>")})
    anafora.evaluate._print_merged_scores(
        file_scores, anafora.evaluate.Scores)
    

def main():
    parser = argparse.ArgumentParser()

    ## Required parameters
    parser.add_argument("--nb_epochs",
                        default=None,
                        type=int,
                        required=True)

    parser.add_argument("--train_batch_size",
                        default=None,
                        type=int,
                        required=True)
    
    parser.add_argument("--eval_batch_size",
                        default=None,
                        type=int,
                        required=True)
    
    parser.add_argument("--lr_rate",
                        default=None,
                        type=float,
                        required=True)

    parser.add_argument("--data_dir",
                        default=None,
                        type=str,
                        required=True)

    args = parser.parse_args()

    tokenizer = AutoTokenizer.from_pretrained("roberta-base", use_fast=True)
    trainer = train(tokenizer, args.nb_epochs, args.train_batch_size, args.eval_batch_size, args.lr_rate, args.data_dir)

    reference_dir = os.path.join(args.data_dir,"test")
    evaluate(tokenizer, trainer, reference_dir,"./predictions_multilabel")


if __name__ == '__main__':
    main()