from transformers import RobertaForTokenClassification
from transformers.modeling_outputs import TokenClassifierOutput
from torch.nn import CrossEntropyLoss
import torch
distances = [
    '-6',
    '-5',
    '-4',
    '-3',
    '-2',
    '-1',
    'None',
    '1',
    '2',
    '3',
    '4',
]
distance_to_index = {l: i for i, l in enumerate(distances)}
index_to_distance = {v: k for k, v in distance_to_index.items()}

weights = [1 if d=='None' else 500 for d in distances]

f = open('./example-data/types.txt')
lines = f.readlines()
types = []
for line in lines:
    types.append(line.replace("\n", ""))

type_to_index = {l: i for i, l in enumerate(types)}
index_to_type = {v: k for k, v in type_to_index.items()}

def load_data(tokenized_dataset, batch_size):
    
    train_dataloader = DataLoader(tokenized_dataset['train'], batch_size=batch_size)
    eval_dataloader = DataLoader(tokenized_dataset['validation'], batch_size=batch_size)
    test_dataloader = DataLoader(tokenized_dataset['test'], batch_size=batch_size)
    
    return train_dataloader, eval_dataloader, test_dataloader

class RobertaForTokenClassificationCustom(RobertaForTokenClassification):
    def forward(
        self,
        input_ids=None,
        attention_mask=None,
        token_type_ids=None,
        position_ids=None,
        head_mask=None,
        inputs_embeds=None,
        labels_type=None,
        labels_distance=None,
        validity_dict=None,
        output_attentions=None,
        output_hidden_states=None,
        return_dict=None,
    ):
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
        
        weight = torch.tensor(weights, dtype=logits.dtype)
        labels_distance = labels_distance.to(torch.long)
        loss_fct = CrossEntropyLoss(weight=weight)
        for i in range(len(labels_distance)):
            for j in range(len(labels_distance[i])):
                token_distance_index = labels_distance.numpy()[i][j]
                token_type_index = labels_type.numpy()[i][j]
                token_distance = index_to_distance[token_distance_index]
                token_type = index_to_type[token_type_index]
                if token_distance != 'None':
                    # check the neighbor tokens' type to see if they can be a valid relation to predict for the original token (labels[i][j]) 
                    indices = []
                    updates = []
                    for index, distance in enumerate(distances): # explore the neighbor tokens
                        if distance == 'None':
                            continue    # because it is the original token itself
                        target_j = j - int(distance) # reverse version of the equation to calculate distances while building the dataset
                        if target_j < 0 or target_j > len(distances) -1: 
                            continue        # if the target_j index is out of limits, pass
                        target_token_type_index = labels_type.numpy()[i][target_j]
                        target_token_type = index_to_type[target_token_type_index]
                        if target_token_type not in validity_dict[token_type]: # check if neighbor token's type if it can be a valid relation
                            logits[i][j][index] = 0                                             # to predict for the original token
        #print(labels_distance.shape)
        labels_distance = torch.reshape(labels_distance, (-1,)) # convert from shape (a, b) to (a*b,)
        #print(labels_distance.shape)
        #print(logits.shape)
        logits = torch.reshape(logits, (-1,logits.shape[-1])) # convert from shape (a, b, c) to (a*b, c)
        #print(logits.shape)
        

        loss = loss_fct(logits, labels_distance)

        if not return_dict:
            output = (logits,) + outputs[2:]
            return ((loss,) + output) if loss is not None else output

        return TokenClassifierOutput(
            loss=loss,
            logits=logits,
            hidden_states=outputs.hidden_states,
            attentions=outputs.attentions,
        )