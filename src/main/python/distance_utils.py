from torch.utils.data import DataLoader
from torchmetrics import Precision, Recall, F1Score
from transformers import get_scheduler
import torch
import numpy as np
from tqdm.auto import tqdm
import os
import math
from distance_custom_models import RobertaForTokenClassificationCustom
<<<<<<< Updated upstream

=======
import anafora
import spacy 
import re
import xml.etree.ElementTree as ET
import inspect
>>>>>>> Stashed changes

def load_data(tokenized_dataset, batch_size):
    
    train_dataloader = DataLoader(tokenized_dataset['train'], batch_size=batch_size)
    eval_dataloader = DataLoader(tokenized_dataset['validation'], batch_size=batch_size)
    test_dataloader = DataLoader(tokenized_dataset['test'], batch_size=batch_size)
    
    return train_dataloader, eval_dataloader, test_dataloader

<<<<<<< Updated upstream
def compute_metrics(predictions, labels, label_list):
    
    label2index = {label:index for index,label in enumerate(label_list)}
    num_labels = len(label_list)
    results = {}
    # Remove ignored index (special tokens)
    true_predictions = [
        [label2index[label_list[p]] for (p, l) in zip(prediction, label) if l != -100]
        for prediction, label in zip(predictions, labels)
    ]
    true_labels = [
        [label2index[label_list[l]] for (p, l) in zip(prediction, label) if l != -100]
        for prediction, label in zip(predictions, labels)
    ]
    flat_true_predictions = [item for sublist in true_predictions for item in sublist]
    flat_true_labels = [item for sublist in true_labels for item in sublist]
    flat_true_predictions = torch.tensor(flat_true_predictions, device=device)
    flat_true_labels = torch.tensor(flat_true_labels, device=device)
    precision_micro = Precision(average='micro').to(device)
    precision_macro = Precision(num_classes=num_labels, average='macro').to(device)
    results["precision_micro"] = round(precision_micro(flat_true_predictions, flat_true_labels).cpu().numpy().item(), 4)
    results["precision_macro"] = round(precision_macro(flat_true_predictions, flat_true_labels).cpu().numpy().item(), 4)
    recall_micro = Recall(average='micro').to(device)
    recall_macro = Recall(num_classes=num_labels, average='macro').to(device)
    results["recall_micro"] = round(recall_micro(flat_true_predictions, flat_true_labels).cpu().numpy().item(), 4)
    results["recall_macro"] = round(recall_macro(flat_true_predictions, flat_true_labels).cpu().numpy().item(), 4)
    f1_micro = F1Score(average='micro').to(device)
    f1_macro = F1Score(num_classes=num_labels, average='macro').to(device)
    results["f1_micro"] = round(f1_micro(flat_true_predictions, flat_true_labels).cpu().numpy().item(), 4)
    results["f1_macro"] = round(f1_macro(flat_true_predictions, flat_true_labels).cpu().numpy().item(), 4)
=======
def compute_metrics(predictions, labels, label_list, device):
    predictions = predictions.long()
    labels = labels.long()
    num_labels = len(label_list)
    results = {}
    precision_micro = Precision(average='micro', ignore_index=6).to(device)
    precision_macro = Precision(num_classes=num_labels, average='macro', ignore_index=6).to(device)
    results["precision_micro"] = round(precision_micro(predictions, labels).cpu().numpy().item(), 4)
    results["precision_macro"] = round(precision_macro(predictions, labels).cpu().numpy().item(), 4)
    recall_micro = Recall(average='micro', ignore_index=6).to(device)
    recall_macro = Recall(num_classes=num_labels, average='macro', ignore_index=6).to(device)
    results["recall_micro"] = round(recall_micro(predictions, labels).cpu().numpy().item(), 4)
    results["recall_macro"] = round(recall_macro(predictions, labels).cpu().numpy().item(), 4)
    f1_micro = F1Score(average='micro', ignore_index=6).to(device)
    f1_macro = F1Score(num_classes=num_labels, average='macro', ignore_index=6).to(device)
    results["f1_micro"] = round(f1_micro(predictions, labels).cpu().numpy().item(), 4)
    results["f1_macro"] = round(f1_macro(predictions, labels).cpu().numpy().item(), 4)
>>>>>>> Stashed changes
    return results

def train_eval_loop(model, train_dataloader, eval_dataloader, num_epochs, learning_rate, label_list, validity_dict):
    num_training_steps = num_epochs * len(train_dataloader)
    num_eval_steps = num_epochs * len(eval_dataloader)
    progress_bar_train = tqdm(range(num_training_steps))
    progress_bar_eval = tqdm(range(num_eval_steps))
    results_to_write = []
    list_train_loss = []
    list_eval_loss = []
    device = torch.device("cuda") if torch.cuda.is_available() else torch.device("cpu") # set it to cpu during debugging
    model.to(device)

    optimizer = torch.optim.AdamW(model.parameters(), lr=learning_rate)
    scheduler = get_scheduler(name="linear", 
                                 optimizer=optimizer, 
                                 num_warmup_steps=0, 
                                 num_training_steps=num_training_steps)
    
    for epoch in range(num_epochs):
        model.train()
        for batch in train_dataloader:
<<<<<<< Updated upstream
            batch = {k: torch.stack(v).to(device) for k, v in batch.items()}
=======
            batch = {k: torch.transpose(torch.stack(v).to(device), 0, 1) for k, v in batch.items()}
>>>>>>> Stashed changes
            outputs = model.forward(input_ids=batch['input_ids'], labels_type=batch['labels_type'], labels_distance=batch['labels_distance'], validity_dict=validity_dict)
            loss = outputs.loss
            loss.backward()
            optimizer.step()
            scheduler.step()
            optimizer.zero_grad()
            progress_bar_train.update(1)
            list_train_loss.append(loss.item())
        
        model.eval()
        eval_predictions = []
        eval_labels = []
        for i, batch in enumerate(eval_dataloader):
<<<<<<< Updated upstream
            batch = {k: torch.stack(v).to(device) for k, v in batch.items()}
=======
            batch = {k: torch.transpose(torch.stack(v).to(device), 0, 1) for k, v in batch.items()}
>>>>>>> Stashed changes
            with torch.no_grad():
                outputs = model.forward(input_ids=batch['input_ids'], labels_type=batch['labels_type'], labels_distance=batch['labels_distance'], validity_dict=validity_dict)
            logits = outputs.logits
            batch_predictions = torch.argmax(logits, dim=-1)
            eval_predictions.append(batch_predictions)
<<<<<<< Updated upstream
            eval_labels.append(batch['labels_distance'])
            progress_bar_eval.update(1)
            list_eval_loss.append(outputs.loss.item())
        
        predictions = torch.cat(tuple(eval_predictions), 0)
        labels = torch.cat(tuple(eval_labels), 0)
        results = compute_metrics(predictions, labels, label_list)
=======
            eval_labels.append(torch.reshape(batch['labels_distance'],(-1,)))
            progress_bar_eval.update(1)
            list_eval_loss.append(outputs.loss.item())
        predictions = torch.cat(tuple(eval_predictions), 0)
        labels = torch.cat(tuple(eval_labels), 0)

        results = compute_metrics(predictions, labels, label_list, device)
>>>>>>> Stashed changes
        print(f"epoch {epoch+1}:", results)
        results_line = "epoch: " + str(epoch+1) + " precision_micro: " + str(results["precision_micro"]) + " precision_macro: " + str(results["precision_macro"]) + " recall_micro: " + str(results["recall_micro"]) + " recall_macro: " + str(results["recall_macro"]) + " f1_micro: " + str(results["f1_micro"]) + " f1_macro: " + str(results["f1_macro"])
        results_to_write.append(results_line)
    return model, results_to_write, list_train_loss, list_eval_loss

<<<<<<< Updated upstream
def test(model_name_or_path, test_dir, output_dir):
    model = RobertaForTokenClassificationCustom.from_pretrained(model_name_or_path)

    for sub_dir, text_file_name, xml_file_name, text in iter_anafora(test_dir):
        sentences = [sentence for sentence in nlp(text).sents]
        inputs = tokenize([sentence.text for sentence in sentences])
        print(f'predicting on {tf.data.Dataset.from_tensor_slices(inputs)}')
        logits = model(inputs)["logits"]
        labels = tf.math.argmax(logits, axis=-1)
        data = anafora.AnaforaData()
        n = 1
        entity = None
        for i, sentence, token_tuples in iter_tokens(inputs, sentences):
            for j, token_id, start, end in token_tuples:
                if token_id not in tokenizer.all_special_ids:
                    label = time_types[labels[i][j]]
                    if label is not None:
                        print(f'{start}:{end} {label} {text[start:end]!r}')
=======
def iter_data(root_dir: str, xml_type: str):
        for dir_path, dir_names, file_names in os.walk(root_dir):
            if not dir_names:

                # read the text from the text file
                [text_file_name] = [f for f in file_names if not f.endswith(".xml")]
                text_path = os.path.join(dir_path, text_file_name)
                with open(text_path) as text_file:
                    text = text_file.read()

                # calculate the XML file name from the text file name
                xml_path = f"{text_path}.TimeNorm.{xml_type}.completed.xml"

                # read the gold annotations or create an empty annotations object
                if xml_type == 'gold':
                    data = anafora.AnaforaData.from_file(xml_path)
                elif xml_type == 'system':
                    data = anafora.AnaforaData()
                else:
                    raise ValueError(f"unsupported xml_type: {xml_type}")

                # generate a tuple for this document
                yield text_path, text, xml_path, data

def iter_tokens(inputs, sentences):
    for sent_index, sentence in enumerate(sentences):
        token_tuples = []
        for token_index in range(inputs["input_ids"].shape[1]):
            offsets = inputs["offset_mapping"][sent_index][token_index].numpy()
            start, end = [0 + o for o in offsets]
            token_id = inputs["input_ids"][sent_index][token_index]
            token_tuples.append((token_index, token_id, start, end))
        yield sent_index, sentence, token_tuples

def _can_merge(text, entity, label, start):
    if entity is None:
        return False
    if label != entity.type:
        return False
    (_, last_end), = entity.spans
    intervening_text = text[last_end:start]
    return not intervening_text or intervening_text.isspace()

def modify_gold_test_data(test_dir, output_dir):
    for text_path, text, xml_path, data in iter_data(test_dir, "gold"):
        text_file_name = text_path.split('/')[-1]
        xml_file_name = xml_path.split('/')[-1]
        print(text_file_name)
        data_to_write = anafora.AnaforaData()
        for entity in data.annotations:
            print("begin:")
            print(entity)
            remove_list = []
            for prop in entity.properties.xml.iter():
                print("text:",prop.text)
                print("type:",type(prop.text))
                if not prop.text:
                    remove_list.append(prop)
            
            for prop in remove_list:
                entity.properties.xml.remove(prop)
            data_to_write.annotations.append(entity)
            
            print(entity)
            print("end")

        output_sub_dir = os.path.join(output_dir, text_file_name)
        if not os.path.exists(output_sub_dir):
            os.makedirs(output_sub_dir, exist_ok=True)
        data_to_write.to_file(os.path.join(output_sub_dir, xml_file_name))
        with open(os.path.join(output_sub_dir, text_file_name), 'w') as f:
            f.write(text)


def test(model, tokenizer, test_dir, output_dir, validity_dict, index_to_distance, relation_to_extract):
    nlp = spacy.load("en_core_web_sm") 
    for text_path, text, xml_path, data in iter_data(test_dir, "gold"):
        text_file_name = text_path.split('/')[-1]
        xml_file_name = xml_path.split('/')[-1]
        print(text_file_name)
        sentences = [sentence for sentence in nlp(text).sents]
        inputs = tokenizer([sentence.text for sentence in sentences],
                                 padding="longest",
                                 return_tensors="pt",
                                 return_offsets_mapping=True) 
        labels_type = torch.from_numpy(np.empty(inputs["input_ids"].shape))
        labels_distance = torch.from_numpy(np.empty(inputs["input_ids"].shape))
        
        with torch.no_grad():
            outputs = model.forward(input_ids=inputs['input_ids'], labels_type=labels_type, labels_distance=labels_distance, validity_dict=validity_dict, test=True)
        logits = outputs.logits
        predictions = torch.argmax(logits, dim=-1)
        doc_token_spans = inputs['offset_mapping']
        start_char_id_to_predicted_start_char_id = {}
        ind = -1
        for sent_i in range(len(doc_token_spans)):
            for token_i in range(len(doc_token_spans[sent_i])):
                prediction = index_to_distance[predictions[sent_i][token_i].cpu().numpy().item()]
                entity_start_in_sentence = doc_token_spans[sent_i][token_i][0].cpu().numpy().item() # sentence level start span
                entity_start_in_doc = entity_start_in_sentence + sentences[sent_i].start_char # document level start span
                
                if prediction != "None": # basically same operation as above, but for the linked entity
                    linked_entity_start_in_sentence = doc_token_spans[sent_i][token_i-int(prediction)][0].cpu().numpy().item()
                    linked_entity_start_in_doc = linked_entity_start_in_sentence + sentences[sent_i].start_char # document level start span
                else:
                    linked_entity_start_in_doc = -1
                
                start_char_id_to_predicted_start_char_id[entity_start_in_doc] = linked_entity_start_in_doc

        start_char_id_to_entity_id_in_doc = {} # this dict contains mapping from document level start span to entity id in a given document
        for entity in data.annotations:
            entity_id = entity.xml.find('id').text.split('@')[0]
            entity_start_char_id = int(entity.xml.find('span').text.split(',')[0])
            start_char_id_to_entity_id_in_doc[entity_start_char_id] = entity_id

        data_to_write = anafora.AnaforaData()
        for entity in data.annotations:
            new_entity = anafora.AnaforaEntity()
            entity_id = entity.xml.find('id').text.split('@')[0]
            entity_start_char_id = int(entity.xml.find('span').text.split(',')[0])
            entity_end_char_id = int(entity.xml.find('span').text.split(',')[1])
            entity_type = entity.xml.find('type').text

            new_entity.id = f"{entity_id}@e@{text_file_name}@system"
            new_entity.spans = (entity_start_char_id, entity_end_char_id),
            new_entity.type = entity_type
            predicted_start_char_id = start_char_id_to_predicted_start_char_id[entity_start_char_id]
            if predicted_start_char_id != -1 and predicted_start_char_id in start_char_id_to_entity_id_in_doc.keys():
                linked_entity_id = start_char_id_to_entity_id_in_doc[predicted_start_char_id]
                new_entity.properties[relation_to_extract] = f"{linked_entity_id}@e@{text_file_name}@system"
            
            data_to_write.annotations.append(new_entity)

        output_sub_dir = os.path.join(output_dir, text_file_name)
        if not os.path.exists(output_sub_dir):
            os.makedirs(output_sub_dir, exist_ok=True)
        xml_file_name = xml_file_name.replace("gold", "system")
        data_to_write.to_file(os.path.join(output_sub_dir, xml_file_name))


        """
        for token_spans in doc_token_spans:
            ind += 1
            for token_span in token_spans:
                start = token_span[0].cpu().numpy().item()
                end = token_span[1].cpu().numpy().item()
                print(sentences[ind].text[start:end])
                print(start)
                print(end)
                print("wow")
        exit()

        
        for token_spans in doc_token_spans:
            for token_span in token_spans:
                start = token_span[0].cpu().numpy().item()
                end = token_span[1].cpu().numpy().item()
                if start == previous_end:
                    token_id -= 1
                for char_index in range(start,end):
                    characters_to_token_ids[char_index] = word_id
                previous_end = end
                token_id += 1
        
        data_to_write = anafora.AnaforaData()
        dict_annotations = {}
        for entity in data.annotations:
            entity_values_per_id = {}
            entity_values_per_start_char_id = {}
            entity_id = entity.xml.find('id').text
            entity_start_char_id = int(entity.xml.find('span').text.split(',')[0])
            entity_type = entity.xml.find('type').text
            if entity_type == "Event": # do not consider event type entities at the moment
                continue
            entity_values_per_id['entity_start_char_id'] = entity_start_char_id
            entity_values_per_id['entity_type'] = entity_type
            entity_values_per_start_char_id['entity_id'] = entity_id
            entity_values_per_start_char_id['entity_type'] = entity_type
            dict_annotations[entity_id] = entity_values_per_id
            dict_annotations[entity_start_char_id] = entity_values_per_start_char_id
        """
        
        
        """
        entity = None

        for i, sentence, token_tuples in iter_tokens(inputs, sentences):
            print(sentence)
            seen_ids = {}
            for j, token_id, start, end in token_tuples:
                if token_id not in tokenizer.all_special_ids:
                    token_distance_label = index_to_distance[predictions[i][j].cpu().numpy().item()]
                    if token_distance_label != "None":
                        #print(f'{start}:{end} {label} {text[start:end]!r}')
                        if _can_merge(text, entity, token_distance_label, start):
                            (start, _), = entity.spans
                        else:
                            entity = anafora.AnaforaEntity()
                            entity_id = characters_to_token_ids[start]
                            if entity_id in seen_ids:
                                continue
                            else:
                                seen_ids.add(entity_id)
                            entity.id = f"{entity_id}@e@{text_file_name}@system"
                            entity.type = "type"
                            linked_entity_id = entity_id - int(token_distance_label)
                            entity.properties[relation_to_extract] = f"{linked_entity_id}@e@{text_file_name}@system"
                            data.annotations.append(entity)
                        entity.spans = (start, end),
        
        """

        """ steve's code for time expressions
        for i, sentence, token_tuples in iter_tokens(inputs, sentences):
            for j, token_id, start, end in token_tuples:
                if token_id not in tokenizer.all_special_ids:
                    label = distances[labels[i][j]]
                    if label:
                        #print(f'{start}:{end} {label} {text[start:end]!r}')
>>>>>>> Stashed changes
                        if _can_merge(text, entity, label, start):
                            (start, _), = entity.spans
                        else:
                            entity = anafora.AnaforaEntity()
                            entity.id = f"{n}@e@{text_file_name}@system"
                            entity.type = label
                            data.annotations.append(entity)
                            n += 1
                        entity.spans = (start, end),
<<<<<<< Updated upstream

        output_sub_dir = os.path.join(output_dir, sub_dir)
        if not os.path.exists(output_sub_dir):
            os.makedirs(output_sub_dir, exist_ok=True)
        xml_file_name = xml_file_name.replace("gold", "system")
        data.to_file(os.path.join(output_sub_dir, xml_file_name))
=======
        """

        
>>>>>>> Stashed changes

def write_results(path, results_to_write):
    results_file = open(os.path.join(path, "all_results_on_eval.txt"), "w")
    for setup, results in results_to_write.items():
        results_file.write(setup + "\n")
        for results_line in results:
            results_file.write(results_line + "\n")
        results_file.write("\n\n")
    results_file.close()

def create_loss_figures(path, loss_log, title):
    plt.rcParams["figure.figsize"] = [17.50, 13.50]
    plt.rcParams["figure.autolayout"] = True
    
    x = np.array([i for i in range(1,len(loss_log)+1)])
    y = np.array(loss_log) 
    
    plt.title(title)
    plt.yticks(np.arange(0, len(loss_log)+1, step=1))
    plt.yticks(np.arange(0, int(math.ceil(max(y))), step=0.05))
    plt.plot(x, y, color="red")
    plt.savefig(path)
    plt.clf()