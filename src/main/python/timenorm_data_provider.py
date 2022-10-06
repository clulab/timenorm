import os
from typing import Tuple, List
import numpy as np
import anafora
import spacy
from transformers import PreTrainedTokenizerFast, AutoTokenizer
import collections
import torch
from sklearn.preprocessing import MultiLabelBinarizer
from datasets import Dataset, DatasetDict

class TimeDataProvider:
    def __init__(self,
                 corpus_dir: str,
                 ) -> None:

        if not os.path.exists(corpus_dir):
            raise Exception(f"The {corpus_dir} does not exit.")
        self.corpus_dir = corpus_dir

    @staticmethod
    def get_time_type_lists(root_dir: str):
        types = []
        operator_types = []
        non_operator_types = []
        f = open(os.path.join(root_dir, 'types.txt'))
        lines = f.readlines()
        for line in lines:
            types.append(line.replace("\n", ""))
        f = open(os.path.join(root_dir, 'operator.txt'))
        lines = f.readlines()
        for line in lines:
            operator_types.append(line.replace("\n", ""))
        f = open(os.path.join(root_dir, 'non-operator.txt'))
        lines = f.readlines()
        for line in lines:
            non_operator_types.append(line.replace("\n", ""))

        return types, operator_types, non_operator_types
    @staticmethod
    def iter_data(root_dir: str, xml_type: str):
        for dir_path, dir_names, file_names in os.walk(root_dir):
            if not dir_names:

                # read the text from the text file
                print(file_names)
                [text_file_name] = [f for f in file_names if not (f.endswith(".xml") or f.endswith(".json") or f.startswith("."))]
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
    
    @staticmethod
    def iter_tokens(inputs, sentences):
        for sent_index, sentence in enumerate(sentences):
            token_tuples = []
            for token_index in range(inputs["input_ids"].shape[1]):
                offsets = inputs["offset_mapping"][sent_index][token_index].numpy()
                start, end = [sentence.start_char + o for o in offsets]
                token_id = inputs["input_ids"][sent_index][token_index]
                token_tuples.append((token_index, token_id, start, end))
            yield sent_index, sentence, token_tuples

    @staticmethod
    def extract_entity_values_from_doc(data, relation_to_extract):
        doc_annotations = {}
        if data:
            for entity in data.annotations:
                entity_values = {}
                entity_id = entity.xml.find('id').text.split('@')[0]
                entity_spans = entity.xml.find('span').text
                entity_type = entity.xml.find('type').text
                #if entity_type == "Event": # do not consider event type entities at the moment
                 #   continue
                related_entity_id = None

                if entity.properties.xml.find(relation_to_extract) is not None:
                    if entity.properties.xml.find(relation_to_extract).text is not None:
                        related_entity_id = entity.properties.xml.find(relation_to_extract).text.split('@')[0] # id of the related entity

                
                entity_values['entity_spans'] = entity_spans
                entity_values['entity_type'] = entity_type
                entity_values['related_entity_id'] = related_entity_id
                
                doc_annotations[entity_id] = entity_values

        return doc_annotations

    def create_dataset_for_split(self,
                                 fast_tokenizer: PreTrainedTokenizerFast,
                                 method: str, # distance or lrd (left-right-doctime)
                                 split: str, # Train, Dev, Test
                                 relation_to_extract: str, 
                                 labels: List[str]): # for this function, set self.corpus to directory that has Train, Dev, Test subdirectories
        types, operator_types, non_operator_types = self.get_time_type_lists(self.corpus_dir)
        sentences = []
        sentence_texts = {}
        sentence_char_labels = {}
        label_to_index = {l: i for i, l in enumerate(labels)}
        type_to_index = {l: i for i, l in enumerate(types)}
        for text_path, text, xml_path, data in self.iter_data(os.path.join(self.corpus_dir, split), "gold"):
            char_labels = collections.defaultdict(set)
            print(text_path)
            tokenized_map = fast_tokenizer(text, return_offsets_mapping=True)
            token_spans = tokenized_map['offset_mapping']
            characters_to_token_ids = {}
            token_id = -1 # to avoid first (0,0) span in offset_mapping
            for token_span in token_spans:
                start, end = token_span
                for char_index in range(start,end):
                    characters_to_token_ids[char_index] = token_id
                token_id += 1
                
            doc_annotations = self.extract_entity_values_from_doc(data, relation_to_extract)
            for entity_id in doc_annotations:
                entity_values = doc_annotations[entity_id]
                entity_type = entity_values['entity_type']
                if(entity_values["related_entity_id"] == None):       # this is the case where entity does not have a relation
                    label = "None"
                    start = int(entity_values['entity_spans'].split(',')[0])
                    end = int(entity_values['entity_spans'].split(',')[1])
                    for i in range(start, end):
                        char_labels[i].add((label, entity_type))

                else:                                                       # this is the case where entity has a relation
                    linked_id = entity_values["related_entity_id"]
                    entity_beginning_char = int(entity_values['entity_spans'].split(',')[0])
                    if linked_id not in doc_annotations.keys():
                        print("linked_id: ",linked_id)
                        print("entity_values: ", entity_values)
                        print("the entity that is linked does not exist in the document")
                        continue
                    linked_entity_values = doc_annotations[linked_id]
                    linked_entity_beginning_char = int(linked_entity_values['entity_spans'].split(',')[0])
                    linked_distance = characters_to_token_ids[entity_beginning_char] - characters_to_token_ids[linked_entity_beginning_char]
                    if method == "lrd":
                        if linked_distance < 0:
                            label = "right"
                        elif linked_distance > 0:
                            label = "left"
                        else:
                            label = "doctime"
                    elif method == "distance":
                        label = str(linked_distance)
                        if label not in labels: # if the distance between 2 entities that have a relation does not exist in predetermined distances (aka the distance is rare) 
                            label = "None"          # the label of the first entity will be none
                    else:
                        print("wrong method name")
                        exit()
                    start = int(entity_values['entity_spans'].split(',')[0])
                    end = int(entity_values['entity_spans'].split(',')[1])
                    for i in range(start, end):
                        char_labels[i].add((label, entity_type))

                    
            # nlp = spacy.load("en_core_web_lg")
            nlp = spacy.load("en_core_web_sm")
            doc = nlp(text)
            for sentence in doc.sents:
                sentences.append(sentence) # all sentences
                sentence_texts[sentence] = text
                sentence_char_labels[sentence] = char_labels 

        inputs = fast_tokenizer([sentence.text for sentence in sentences],
                                 padding="longest",
                                 return_tensors="pt",
                                 return_offsets_mapping=True) 

        # labels = np.empty(inputs["input_ids"].shape + (2,)) # added (2,) to get labels and types together in a list
        labels_type = np.empty(inputs["input_ids"].shape)
        labels_location = np.empty(inputs["input_ids"].shape)

        for i, sentence, token_tuples in self.iter_tokens(inputs, sentences): # go back to raw texts, 
                                                                         # and for each wordpiece assign a label: 
                                                                         # None if its not annotated
            text = sentence_texts[sentence]                              # None if annotated and no relation 
            char_labels = sentence_char_labels[sentence]                 # distance/lrd if annotated and relation exist
            for j, token_id, start, end in token_tuples:
                # sanity check for mismatch between text and word-piece
                word_piece = fast_tokenizer.decode(token_id)
                if token_id not in fast_tokenizer.all_special_ids and text[start:end] != word_piece.lstrip(' '):
                    raise ValueError(f"{text[start:end]!r} != {word_piece!r}")

                # find labels for the given offsets
                
                
                token_labels = {x for c in range(start, end) 
                                for x, _ in char_labels[c]} # there was "or {None} here, i removed it"
                
                token_types = {x for c in range(start, end) 
                                for _, x in char_labels[c]}
                if not token_labels:
                    token_label = "None"
                elif len(token_labels) == 1:
                    token_label = token_labels.pop()
                else:                   
                    context = f"{text[start-5:start]}[{text[start:end]}]"\
                              f"{text[end:end+5]}"
                    print(f"Skipping token labels: {context!r} {token_labels}")
                    token_label = "None"
                if not token_types:
                    token_type = "None"
                elif len(token_types) == 1:
                    token_type = token_types.pop()
                else:               # if there is more than one type throw away the operator ones and take the first non operator as type
                    token_type = "None"
                    for _token_type in token_types:
                        if _token_type in non_operator_types:
                            token_type = _token_type
                            break
                    if token_type == "None":
                        print("none of the assigned types are non-operator, can't choose, so set type to None.")
                        context = f"{text[start-5:start]}[{text[start:end]}]"\
                                  f"{text[end:end+5]}"
                        print(f"Skipping token types: {context!r} {token_types}")
                    
                        
                    
                
                # labels[i][j] = [label_to_index[token_label], type_to_index[token_type]] 
                labels_type[i][j] = type_to_index[token_type]
                labels_location[i][j] = label_to_index[token_label]
            
            if i%100 == 0:    
                print(f'{i}/{len(sentences)} is done')
        labels_type = torch.from_numpy(labels_type)
        labels_location = torch.from_numpy(labels_location)
        split_dict = {}
        split_dict['input_ids'] = inputs['input_ids']
        split_dict['labels_type'] = labels_type
        split_dict['labels_location'] = labels_location

        return Dataset.from_dict(split_dict)

    
    def read_data_to_lrd_format(self,
                               fast_tokenizer: PreTrainedTokenizerFast,
                               relation_to_extract: str,
                               labels: List[str]):
        dataset_dict = {}
        dataset_dict['train'] = self.create_dataset_for_split(fast_tokenizer, "lrd", 'Train', relation_to_extract, labels)
        dataset_dict['validation'] = self.create_dataset_for_split(fast_tokenizer, "lrd", 'Dev', relation_to_extract, labels)
        dataset_dict['test'] = self.create_dataset_for_split(fast_tokenizer, "lrd", 'Test', relation_to_extract, labels)

        return DatasetDict(dataset_dict)
    
    def read_data_to_distance_format(self,
                                   fast_tokenizer: PreTrainedTokenizerFast,
                                   relation_to_extract: str,
                                   labels: List[str]):
        dataset_dict = {}
        dataset_dict['train'] = self.create_dataset_for_split(fast_tokenizer, "distance", 'Train', relation_to_extract, labels)
        dataset_dict['validation'] = self.create_dataset_for_split(fast_tokenizer, "distance", 'Dev', relation_to_extract, labels)
        dataset_dict['test'] = self.create_dataset_for_split(fast_tokenizer, "distance", 'Test', relation_to_extract, labels)

        return DatasetDict(dataset_dict)

    def read_data_to_multi_label_format(self, fast_tokenizer:  PreTrainedTokenizerFast, types:List[str], max_length:int, eval=False) -> Tuple[List[str], List[str]]:
        """Read the data and format it for multi label token classification"""

        label_to_index = {l: i for i, l in enumerate(types)}
        mlb = MultiLabelBinarizer().fit([list(label_to_index.values())])
        
        inputs = {
          "input_ids":[],
          "attention_mask":[]
        }
        if eval is True: inputs["offset_mapping"]= []
        labels = []
        
        def convert_to_onehot(tags):
          """convert the list of integer labels to one hot vector format"""
          converted = mlb.transform(tags)
          return converted[:,1:] #exclude None label 
        
        for _, text, _, data in self.iter_data(self.corpus_dir, "gold"):

            #convert data into dict format: key - span, value - entity types
            entity_values = {}
            for entity in data.annotations:
                entity_spans = entity.xml.find('span').text
                start, end = [int(index) for index in entity_spans.split(',')]
                entity_type = entity.xml.find('type').text
                if entity_type == "Event": # do not consider event type entities at the moment
                    continue
                if (start,end) not in entity_values:
                  entity_values[(start,end)] = [entity_type]
                elif entity_type not in entity_values[(start,end)]: #not allowing duplicates
                  entity_values[(start,end)].append(entity_type)
            
         
            #split the text into sentences and tokenize them
            nlp = spacy.load("en_core_web_lg")
            doc = nlp(text)

            for sentence in doc.sents:
                tags = [] #labels for this sentence
                sentence_start = sentence.start_char
                tokenized_input = fast_tokenizer (sentence.text,max_length=max_length, padding='max_length', truncation=True,return_offsets_mapping=True)

                token_offsets = tokenized_input["offset_mapping"]
                for offset in token_offsets:
                    tok_start, tok_end = sentence_start+offset[0], sentence_start+offset[1]
                    label = ["None"]
                    if (tok_start, tok_end) in entity_values:
                      label = entity_values.pop((tok_start, tok_end))
                    #see if this span is a subword
                    else:
                      for gold_span in entity_values:
                          gold_span_start, gold_span_end = gold_span
                          if gold_span_start<=tok_start and tok_end<=gold_span_end:
                              label = entity_values.pop(gold_span)
                              break
                    tags.append([label_to_index[l] for l in label])
                    
                # append sentence level data
                labels.append(convert_to_onehot(tags))
                inputs["input_ids"].append(torch.tensor(tokenized_input["input_ids"]))
                inputs["attention_mask"].append(torch.tensor(tokenized_input["attention_mask"]))
                if eval is True:
                    inputs["offset_mapping"].append(torch.tensor(tokenized_input["offset_mapping"]))

        #convert list to tensors
        for key in inputs: 
          inputs[key] = torch.stack(inputs[key],dim=0) 
        labels = torch.from_numpy(np.asarray(labels)).to(torch.float)
        
        return inputs,labels

    def read_data_to_operator_format(self, fast_tokenizer:  PreTrainedTokenizerFast, types:List[str], max_length:int, eval=False) -> Tuple[List[str], List[str]]:
        """
        Read the data and format it for multi-class classification 
        :param types labels (i.e., operator or non-operator) of the multi class classifier
        :param eval  whether the tokenizer will return offsets that will be needed in evaluation
        """
        def get_type_from_file(FILENAME):
            types = []
            with open(FILENAME,'r') as f:
                lines = f.readlines()
            for line in lines:
                types.append(line.replace("\n", ""))
            return types

        operator = get_type_from_file('operator.txt')
        non_operator = get_type_from_file('non-operator.txt')
        op_bio_label_to_index = {**{"B-"+l: i+1 for i, l in enumerate(operator)}, **{"I-"+l: i+len(operator)+1 for i, l in enumerate(operator)}}
        nonop_bio_label_to_index = {**{"B-"+l: i+1 for i, l in enumerate(non_operator)}, **{"I-"+l: i+len(operator)+1 for i, l in enumerate(non_operator)}}

        label_to_index = {l: i+1 for i, l in enumerate(types)} #None label index is 0 
        bio_label_to_index = op_bio_label_to_index if types == operator else nonop_bio_label_to_index

        inputs = {
            "input_ids":[],
            "attention_mask":[]
        }
        if eval is True: inputs["offset_mapping"]= []
        labels = []
        bio_labels = []

        for _, text, _, data in self.iter_data(self.corpus_dir, "gold"):
            #convert data into dict format: key - span, value - entity types
            entity_values = {}
            for entity in data.annotations:
                entity_spans = entity.xml.find('span').text
                start, end = [int(index) for index in entity_spans.split(',')]
                entity_type = entity.xml.find('type').text
                if entity_type == "Event": # do not consider event type entities at the moment
                    continue
                if (start,end) not in entity_values:
                    entity_values[(start,end)] = [entity_type]
                elif entity_type not in entity_values[(start,end)]: #not allowing duplicates
                    entity_values[(start,end)].append(entity_type)
            
            #split the text into sentences and tokenize them
            nlp = spacy.load("en_core_web_lg")
            doc = nlp(text)

            for sentence in doc.sents:
                prev_label = None 

                tags = [] #labels for this sentence
                bio_tags = [] 
                
                sentence_start = sentence.start_char
                tokenized_input = fast_tokenizer (sentence.text,max_length=max_length, padding='max_length', truncation=True,return_offsets_mapping=True)
                token_offsets = tokenized_input["offset_mapping"]
                for offset in token_offsets:
                    tok_start, tok_end = sentence_start+offset[0], sentence_start+offset[1]
                    label = ["None"]
                    if (tok_start, tok_end) in entity_values:
                        label = entity_values.pop((tok_start, tok_end))
                    #see if this span is a subwords
                    else:
                        for gold_span in entity_values:
                            gold_span_start, gold_span_end = gold_span
                            if gold_span_start<=tok_start and tok_end<=gold_span_end:
                                label = entity_values.pop(gold_span)
                                break

                    tok_label = [label_to_index[l] if l in label_to_index else 0 for l in label]
                    tags.append(tok_label[0])

                    tok_bio_label = [l for l in label]
                    #decide inside/outside for BIO format
                    if tok_bio_label[0]!="None" and tok_bio_label[0] in label_to_index:
                        if tok_bio_label[0] == prev_label:
                            bio_tags.append(bio_label_to_index["I-"+tok_bio_label[0]])
                        else:
                            bio_tags.append(bio_label_to_index["B-"+tok_bio_label[0]])
                    else:
                        bio_tags.append(0) #None label 
                    prev_label = tok_bio_label[0] #update

                #append sentence level data
                labels.append(tags)
                bio_labels.append(bio_tags)

                inputs["input_ids"].append(torch.tensor(tokenized_input["input_ids"]))
                inputs["attention_mask"].append(torch.tensor(tokenized_input["attention_mask"]))
                if eval is True:
                    inputs["offset_mapping"].append(torch.tensor(tokenized_input["offset_mapping"]))

        #convert list to tensors
        for key in inputs: 
            inputs[key] = torch.stack(inputs[key],dim=0)

        labels = torch.from_numpy(np.asarray(labels))
        labels = labels.type(torch.LongTensor)

        bio_labels = torch.from_numpy(np.asarray(bio_labels))
        bio_labels = labels.type(torch.LongTensor)

        return inputs,labels, bio_labels

    def read_data_to_pinter_seqs_format(self,
                                        fast_tokenizer: PreTrainedTokenizerFast,
                                        max_length: int) -> Tuple[List[str], List[str]]:
        """Read the text and Anafora annotations to source and target sequence
        The source sequence is plain text.
        The format of the target sequence is: ... <m> <ptr{entity_mention_token_index}> </m> <t> {entity_type} </t> ...
        """
        all_src_seqs = []
        all_tgt_seqs = []

        for _, text, _, data in self.iter_data(self.corpus_dir, "gold"):
            spans_types = {
                span: annotation.type
                for annotation in data.annotations
                for span in annotation.spans
            }

            # Sort the annotations by start span
            def get_start_offset(span_type):
                (_start, _end), types = span_type
                return _start

            spans_types = dict(sorted(spans_types.items(), key=get_start_offset))

            # Split the document into sentences, performed by DependencyParser in spacy
            nlp = spacy.load("en_core_web_sm")
            doc = nlp(text)

            for sent in doc.sents:
                sent_start_char, sent_end_char = sent.start_char, sent.end_char

                # Tokenize the sentence and get char level offsets
                tokenized_features = fast_tokenizer(sent.text,
                                                    return_offsets_mapping=True,
                                                    add_special_tokens=False)
                tokens_char_offsets = tokenized_features['offset_mapping']

                # Create the mapping
                char_offset_to_token_index = [-1 for _ in sent.text]

                # For each token's start and end offset
                for i, (start, end) in enumerate(tokens_char_offsets):

                    # fill the token index from start to end in the list
                    for j in range(start, end):
                        char_offset_to_token_index[j] = i

                # target seq
                tgt_seq = []

                # Find the annotations spans corresponds to this sentence
                for span, entity_type in spans_types.items():
                    if span[0] >= sent_start_char and span[1] <= sent_end_char:

                        # Get the entity's span for the entity in current sentence
                        entity_sent_char_start = span[0] - sent_start_char
                        entity_sent_char_end = span[1] - sent_start_char

                        # Left close right open for indexing
                        entity_sent_char_end -= 1

                        # start and end token index
                        start_token_index = char_offset_to_token_index[entity_sent_char_start]
                        end_token_index = char_offset_to_token_index[entity_sent_char_end]

                        # Make sure the entity's spans are less than our max_length -1
                        if start_token_index >= max_length - 1 or end_token_index >= max_length - 1:
                            continue

                        # Assert start and end position index is not -1
                        assert start_token_index != -1
                        assert end_token_index != -1

                        # create the pointer tokens
                        ptr_tokens = " ".join([f"<ptr{i}>" for i in range(start_token_index, end_token_index + 1)])
                        ptr_tokens = f" <m> {ptr_tokens} </m> <t> {entity_type} </t>"
                        tgt_seq.append(ptr_tokens)
                all_tgt_seqs.append(''.join(tgt_seq))
                all_src_seqs.append(sent.text)

        return all_src_seqs, all_tgt_seqs
