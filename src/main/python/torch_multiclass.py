import os
import torch
import transformers
from typing import Tuple, List
import numpy as np
import anafora, anafora.evaluate 
import spacy
from transformers import PreTrainedTokenizerFast,AutoTokenizer
from sklearn.preprocessing import MultiLabelBinarizer
from torch.utils.data import Dataset
from transformers import RobertaForTokenClassification,RobertaForTokenClassification, Trainer, TrainingArguments
import torch.nn as nn
import collections
import argparse
from typing import List, Tuple
from sklearn.utils import compute_class_weight
import datasets
from timenorm_data_provider import TimeDataProvider

#GPU set up 
from torch import cuda
device = 'cuda' if cuda.is_available() else 'cpu'

nlp = spacy.load("en_core_web_lg")

#load necessary data
def get_type_from_file(FILENAME):
    types = []
    with open(FILENAME,'r') as f:
        lines = f.readlines()
    for line in lines:
        types.append(line.replace("\n", ""))
    return types

types = get_type_from_file('types.txt')
operator = get_type_from_file('operator.txt')
non_operator = get_type_from_file('non-operator.txt')

op_bio_label_to_index = {**{"B-"+l: i+1 for i, l in enumerate(operator)}, **{"I-"+l: i+len(operator)+1 for i, l in enumerate(operator)}}
nonop_bio_label_to_index = {**{"B-"+l: i+1 for i, l in enumerate(non_operator)}, **{"I-"+l: i+len(operator)+1 for i, l in enumerate(non_operator)}}

#custom dataset 
class MulticlassDataset(Dataset):

    def __init__(self, encodings, labels):
      self.encodings = encodings
      self.labels = labels

    def __getitem__(self, idx):
        item = {key: val[idx] for key, val in self.encodings.items()}
        item['labels'] = self.labels[idx]
        return item

    def __len__(self):
        return len(self.labels)

def iter_anafora(anafora_root_dir):
    for sub_dir, text_file_name, [xml_file_name] in anafora.walk(
            anafora_root_dir, "TimeNorm[.]gold[.]completed[.]xml$"):
        text_path = os.path.join(anafora_root_dir, sub_dir, text_file_name)
        with open(text_path) as text_file:
            text = text_file.read()
        yield sub_dir, text_file_name, xml_file_name, text.rstrip()

def _can_merge(text, entity, label, start):
    if entity is None:
        return False
    if label != entity.type:
        return False
    (_, last_end), = entity.spans
    intervening_text = text[last_end:start]
    return not intervening_text or intervening_text.isspace()

def iter_tokens( inputs, sentences):
    for sent_index, sentence in enumerate(sentences):
        token_tuples = []
        for token_index in range(inputs["input_ids"].shape[1]):
            offsets = inputs["offset_mapping"][sent_index][token_index].numpy()
            start, end = [sentence.start_char + o for o in offsets]
            token_id = inputs["input_ids"][sent_index][token_index]
            token_tuples.append((token_index, token_id, start, end))
        yield sent_index, sentence, token_tuples

def get_class_weights(types, flattened_labels):
  """computes the class weight and returns a list to account for class imbalance """
  class_weights=compute_class_weight( class_weight ='balanced',classes = np.unique(flattened_labels),y = flattened_labels.numpy())
  class_weight_dict = dict(zip(np.unique(flattened_labels), class_weights))
  #for labels that do not exist, manually assign weight 1
  total_class_weights= []
  for idx in range(len(types)+1):
    if idx not in class_weight_dict:
      total_class_weights.append(1) #manually assign weight 1
    else:
      total_class_weights.append(class_weight_dict[idx]) #give the assigned weight 

  total_class_weights =torch.tensor(total_class_weights,dtype=torch.float).to(device)
  return total_class_weights

def create_custom_trainer(class_weights):
  """creates custom trainer that accounts for class imbalance"""
  class CustomTrainer(Trainer):
      def compute_loss(self, model, inputs, return_outputs=False):
          labels = inputs.get("labels")
          # forward pass
          outputs = model(**inputs)
          logits = outputs.get("logits")
          # compute custom loss 
          loss_fct = nn.CrossEntropyLoss(weight=class_weights)
          loss = loss_fct(logits.view(-1, self.model.config.num_labels), labels.view(-1))
          return (loss, outputs) if return_outputs else loss
  return CustomTrainer

def build_compute_metrics(types):
  types = ["O"] + types
  def compute_metrics(eval_preds):
      metrics = datasets.load_metric("seqeval")
      logits, labels = eval_preds
      predictions = np.argmax(logits, axis=-1)
      #convert to BIO format
      true_labels = [[types[tok] for tok in sent] for sent in labels]
      true_pred= [[types[tok] for tok in sent] for sent in predictions]
      return metrics.compute(predictions=true_pred, references=true_labels)
  return compute_metrics

def train( types, tokenizer, num_epochs, train_batch_size, eval_batch_size,lr_rate, data_dir):

    train_dir = os.path.join(data_dir,"train")
    dev_dir = os.path.join(data_dir,"dev")

    train_data_loader = TimeDataProvider(corpus_dir=train_dir)
    val_data_loader = TimeDataProvider(corpus_dir=dev_dir)

    #prepare train/val dataset
    train_inputs, train_labels, train_bio_labels = train_data_loader.read_data_to_operator_format(tokenizer, types=types,max_length=128)
    train_dataset = MulticlassDataset(train_inputs, train_bio_labels)

    val_inputs, val_labels, val_bio_labels =val_data_loader.read_data_to_operator_format(tokenizer, types=types,max_length=128)
    val_dataset = MulticlassDataset(val_inputs, val_bio_labels)

    bio_labels = list(op_bio_label_to_index.keys()) if types == operator else list(nonop_bio_label_to_index.keys())
    class_weights = get_class_weights(bio_labels, torch.flatten(train_bio_labels))

    output_dir = "./results_op" if types==operator else "./results_nonop"
    training_args = TrainingArguments(
        output_dir=output_dir,          # output directory
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
    model =  RobertaForTokenClassification.from_pretrained('roberta-base', num_labels=len(bio_labels)+1)

    #load our custom trainer that accounts for class imbalance
    CustomTrainer = create_custom_trainer(class_weights)

    trainer = CustomTrainer(
        model=model,                         
        args=training_args,                  
        train_dataset=train_dataset,         
        eval_dataset=val_dataset             
    )
    trainer.compute_metrics = build_compute_metrics(bio_labels)

    trainer.train()
    
    return trainer

def evaluate(types, tokenizer, trainer, reference_dir, output_dir):
    gold_labels= list(op_bio_label_to_index.keys()) if types == operator else list(nonop_bio_label_to_index.keys())
    gold_labels = ["None"] + gold_labels
    for sub_dir, text_file_name, xml_file_name, text in iter_anafora(reference_dir):
        sentences = [sentence for sentence in nlp(text).sents]

        #write the prediction of the test set
        test_provider = TimeDataProvider(corpus_dir=os.path.join(reference_dir,sub_dir))
        test_inputs, test_labels, test_bio_labels = test_provider.read_data_to_operator_format(tokenizer, types=types,max_length=128)
        test_dataset = MulticlassDataset(test_inputs, test_bio_labels)
        logits = trainer.predict(test_dataset)[0] #get the logits 
        labels = np.argmax(logits,axis=-1)
        data = anafora.AnaforaData()
        n = 1
        entity = None
        #iterate through the sentences of this document 

        test_inputs_w_offsets = test_provider.read_data_to_operator_format(tokenizer, types=types,max_length=128,eval=True)[0]
        for i, sentence, token_tuples in iter_tokens(test_inputs_w_offsets, sentences):
            for j, token_id, start, end in token_tuples:
                if token_id not in tokenizer.all_special_ids:
                    pred = gold_labels[labels[i][j]]
                    if pred != "None":
                        print(f'{start}:{end} {pred} {text[start:end]!r}')
                        pred = pred[2:] #strip off the BIO tag
                        if _can_merge(text, entity, pred, start):
                            (start, _), = entity.spans
                        else:
                            entity = anafora.AnaforaEntity()
                            entity.id = f"{n}@e@{text_file_name}@system"
                            entity.type = pred
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
    toExclude = {("Event", "<span>"), ("NotNormalizable", "<span>"),("Modifier", "<span>")}
    if types == operator:
        toExclude.update(set(non_operator)) #exclude non_operator in evaluation
    else:
        toExclude.update(set(operator))

    file_scores =anafora.evaluate.score_dirs(reference_dir, output_dir,include={("*", "<span>")}, exclude=toExclude)
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

    #train two separate models: one for operators, the other for non-operators
    optrainer = train(operator,tokenizer, args.nb_epochs, args.train_batch_size, args.eval_batch_size, args.lr_rate,args.data_dir)
    nonoptrainer = train(non_operator, tokenizer, args.nb_epochs, args.train_batch_size, args.eval_batch_size, args.lr_rate, args.data_dir)
    
    reference_dir = os.path.join(args.data_dir,"test")
    evaluate(operator,tokenizer, optrainer,reference_dir,"./predictions_operator")
    evaluate(non_operator, tokenizer, nonoptrainer, reference_dir, "./predictions_nonoperator")



if __name__ == '__main__':
    main()
