import os
from typing import Tuple, List
import numpy as np
import anafora
import spacy
from transformers import PreTrainedTokenizerFast, AutoTokenizer
import collections

class TimeDataProvider:
    def __init__(self,
                 corpus_dir: str,
                 ) -> None:

        if not os.path.exists(corpus_dir):
            raise Exception(f"The {corpus_dir} does not exit.")
        self.corpus_dir = corpus_dir

    @staticmethod
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
    
    @staticmethod
    def iter_tokens(self, inputs, sentences):
        for sent_index, sentence in enumerate(sentences):
            token_tuples = []
            for token_index in range(inputs["input_ids"].shape[1]):
                offsets = inputs["offset_mapping"][sent_index][token_index].numpy()
                start, end = [sentence.start_char + o for o in offsets]
                token_id = inputs["input_ids"][sent_index][token_index]
                token_tuples.append((token_index, token_id, start, end))
            yield sent_index, sentence, token_tuples

    @staticmethod
    def extract_entity_values_from_doc(self, data, relation_to_extract):
        doc_annotations = {}
        if data:
            for entity in data.annotations:
                entity_values = {}
                entity_id = entity.xml.find('id').text.split('@')[0]
                entity_spans = entity.xml.find('span').text
                entity_type = entity.xml.find('type').text
                if entity_type == "Event": # do not consider event type entities at the moment
                    continue
                related_entity_id = None

                if entity.properties.xml.find(relation_to_extract) is not None:
                    if entity.properties.xml.find(relation_to_extract).text is not None:
                        related_entity_id = entity.properties.xml.find(relation_to_extract).text.split('@')[0] # id of the related entity
                
                entity_values['entity_spans'] = entity_spans
                entity_values['entity_type'] = entity_type
                entity_values['related_entity_id'] = related_entity_id
                
                doc_annotations[entity_id] = entity_values

        return doc_annotations

    def read_data_to_distance_format(self, 
                                     fast_tokenizer: PreTrainedTokenizerFast,
                                     relation_to_extract: str, 
                                     distances: List[str],
                                     types: List[str]):
        sentences = []
        sentence_texts = {}
        sentence_char_labels = {}
        label_to_index = {l: i for i, l in enumerate(distances)}
        type_to_index = {l: i for i, l in enumerate(types)}
        for text_path, text, xml_path, data in self.iter_data(self.corpus_dir, "gold"):
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
                        print("the entity that is linked does not exist in the document")
                        continue
                    linked_entity_values = doc_annotations[linked_id]
                    linked_entity_beginning_char = int(linked_entity_values['entity_spans'].split(',')[0])
                    linked_distance = characters_to_token_ids[entity_beginning_char] - characters_to_token_ids[linked_entity_beginning_char]
                
                    label = str(linked_distance)
                    if label not in distances: # if the distance between 2 entities that have a relation does not exist in predetermined distances (aka the distance is rare) 
                        label = "None"          # the label of the first entity will be none
                    
                    start = int(entity_values['entity_spans'].split(',')[0])
                    end = int(entity_values['entity_spans'].split(',')[1])
                    for i in range(start, end):
                        char_labels[i].add((label, entity_type))

                    
            nlp = spacy.load("en_core_web_lg")
            doc = nlp(text)
            for sentence in doc.sents:
                sentences.append(sentence) # all sentences
                sentence_texts[sentence] = text
                sentence_char_labels[sentence] = char_labels 

        inputs = fast_tokenizer([sentence.text for sentence in sentences],
                                 padding="longest",
                                 return_tensors="pt",
                                 return_offsets_mapping=True) 

        labels = np.empty(inputs["input_ids"].shape + (2,)) # added (2,) to get labels and types together in a list

        for i, sentence, token_tuples in self.iter_tokens(inputs, sentences): # go back to raw texts, 
                                                                         # and for each wordpiece assign a label: 
                                                                         # None if its not annotated
            text = sentence_texts[sentence]                              # None if annotated and no relation 
            char_labels = sentence_char_labels[sentence]                 # distance if annotated and relation exist
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
                else:
                    context = f"{text[start-5:start]}[{text[start:end]}]"\
                              f"{text[end:end+5]}"
                    print(f"Skipping token types: {context!r} {token_types}")
                    token_type = "None"
                    
                
                labels[i][j] = [label_to_index[token_label], type_to_index[token_type]] 
            
            if i%100 == 0:    
                print(f'{i}/{len(sentences)} is done')
        print("dataset has been created")
        
        return inputs, labels
        """
        return tf.data.Dataset.from_tensor_slices((
            dict(inputs),
            tf.constant(labels),
        ))
        """

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
