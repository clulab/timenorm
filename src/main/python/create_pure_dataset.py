# CREATE DATASET FOR PURE 

import anafora
import os 
import matplotlib.pyplot as plt
import numpy as np 
import operator
import re
import spacy
import collections
import json


def iter_data(root_dir: str, xml_type: str):
    for dir_path, dir_names, file_names in os.walk(root_dir):
        if not dir_names:

            # read the text from the text file
            [text_file_name] = [f for f in file_names if not f.endswith(".xml") and not f.endswith(".json") and not f.endswith(".DS_Store")]
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

output_dir = "/Users/bulut/timenorm-garage/tempeval-2013-replicate-PURE/"
if os.path.exists(output_dir):
    pass
else:
    os.mkdir(output_dir)
splits = ['Train', 'Dev', 'Test']
for split in splits:

    original_dir = "/Users/bulut/timenorm-garage/tempeval-2013-replicate/" + split
    combined = {} # saves text and annotations of a single text file in each key
    nlp = spacy.load("en_core_web_md")
    #infixes = [":", "/", "-"] + nlp.Defaults.infixes
    infixes = nlp.Defaults.infixes + [":", "/", "-", 
                                      r"(?<=\d)(?=[{spacy.lang.char_classes.ALPHA_UPPER}])", # 08EDT
                                      r"(?<=\b\d{2})(?=\d{2}\s?[{spacy.lang.char_classes.ALPHA_UPPER}])", #0804EDT
                                      r"(?<=\b\d{4})(?=\d{4}\b)",
                                      r"(?<=\b\d{6})(?=\d{2}\b)"] 


    infix_regex = spacy.util.compile_infix_regex(infixes)
    nlp.tokenizer.infix_finditer = infix_regex.finditer

    # get text and corresponding annotation for each file and store them in a dict with filenames as keys
    for text_path, text, xml_path, data in iter_data(original_dir, "gold"):
            text_file_name = text_path.split('/')[-1]
            combined[text_file_name] = {"text": text, "annotation": data}

    # get spans from tokenizer output
    all_doc_spans = {}
    for k,v in combined.items():
        doc_spans = {}
        text = v['text']
        sentences = list(nlp(text).sents)
        ctr = 0
        for i, sentence in enumerate(sentences):
            sent_spans = []
            for j, token in enumerate(sentence):
                # get token offsets from Spacy
                if len(set(token.text)) == 1 and set(token.text).pop() == '\n': # if a token consists of linebreaks only
                    continue
                start = token.idx
                end = start + len(token.text)
                sent_spans.append((token, start, end, ctr))
                ctr += 1
            doc_spans[i] = sent_spans # for each sentence
        all_doc_spans[k] = doc_spans # for each file


    # get actual spans from original annotations
    all_annotated_doc_spans = {}
    for k,v in combined.items():
        annotated_doc_spans = [] 
        for entity in v["annotation"].annotations:
            entity_spans = entity.xml.find('span').text
            entity_type = entity.xml.find('type').text
            start, end = [int(index) for index in entity_spans.split(',')]
            # entity = v["text"][start:end]
            annotated_doc_spans.append((start,end, entity_type))
        all_annotated_doc_spans[k] = annotated_doc_spans # for each file. can't do for each sentence as the original annotations are not annotated by sentence



    # create a dictionary: token indexes and token types for each sentence in each file where tokenizer output spans match annotated spans
    all_matching_token_info_per_sent = {}
    all_matching_token_info_per_doc = {}
    for filename, annotated_doc_spans in all_annotated_doc_spans.items(): # traverse through all original annotated entities and their spans
        matching_token_info_per_sent = {}
        matching_token_info_per_doc = {}
        doc_spans = all_doc_spans[filename] # get tokenizer's entity spans into doc_spans from all_doc_spans
        for sent_i, sent_spans in doc_spans.items(): # traverse tokenizer's entity spans sentence by sentence 
            matching_token_info_per_sent[sent_i] = {}
            for entity_span in sent_spans:
                token, start, end, token_index = entity_span # for each entity
                for annotated_doc_span in annotated_doc_spans: # traverse through original annotated entities
                    annotated_start, annotated_end, annotated_type = annotated_doc_span
                    if annotated_start==start:
                        if annotated_end==end: # if tokenizer output spans match with original annotated spans 
                            matching_token_info_per_sent[sent_i][(start, end)] = (token_index, annotated_type)
                            matching_token_info_per_doc[(start, end)] = (sent_i, token_index, annotated_type)
                        # write the code to create multi token structure, after trying PURE system

        all_matching_token_info_per_sent[filename] = matching_token_info_per_sent
        all_matching_token_info_per_doc[filename] = matching_token_info_per_doc


    # create a dictionary to store entity info: id to spans
    all_entity_info = {}
    for k,v in combined.items():
        doc_entity_info = {}
        for entity in v["annotation"].annotations:
            entity_id = entity.xml.find('id').text.split('@')[0]
            entity_spans = entity.xml.find('span').text
            start, end = [int(index) for index in entity_spans.split(',')]
            doc_entity_info[entity_id] = (start, end)
        all_entity_info[k] = doc_entity_info

    # get actual relations from original annotations
    all_annotated_relations = {}
    for k,v in combined.items():
        annotated_doc_relations = [] 
        print("filename: ", k)
        for entity in v["annotation"].annotations.xml:
            entity_id = None
            relation = None
            related_entity_id = None
            for item in entity.iter():
                if item.text and '@' in item.text:
                    if item.tag == 'id':
                        entity_id = item.text.split('@')[0]
                    else:
                        relation = item.tag
                        related_entity_id = item.text.split('@')[0]
            if relation:
                start, end = all_entity_info[k][entity_id]
                start_related, end_related = all_entity_info[k][related_entity_id]
                annotated_doc_relations.append((start, end, start_related, end_related, relation))
        all_annotated_relations[k] = annotated_doc_relations

    # create a dictionary: token indexes and relations for each sentence in each file where tokenizer output spans match annotated spans and there is a relation
    all_matching_relation_info = {}
    for filename, annotated_doc_relations in all_annotated_relations.items():
        matching_relation_info = {}
        for sent_i in all_matching_token_info_per_sent[filename].keys():
            matching_relation_info[sent_i] = []
        for annotated_entity_relation in annotated_doc_relations:
            annotated_start, annotated_end, annotated_start_related, annotated_end_related, relation = annotated_entity_relation
            if (annotated_start, annotated_end) in all_matching_token_info_per_doc[filename].keys():
                sent_i, token_index, annotated_type = all_matching_token_info_per_doc[filename][(annotated_start, annotated_end)]
            else:
                print("mismatched span, skipping this one")
                continue
            if (annotated_start_related, annotated_end_related) in all_matching_token_info_per_doc[filename].keys():
                related_sent_i, related_token_index, related_annotated_type = all_matching_token_info_per_doc[filename][(annotated_start_related, annotated_end_related)]
            else:
                print("mismatched span, skipping this one")
                continue
            if sent_i != related_sent_i:
                print("this isn't supposed to happen, if happens we're skippin'")
                continue
            matching_relation_info[sent_i].append((token_index, token_index, related_token_index, related_token_index, relation))
        all_matching_relation_info[filename] = matching_relation_info

    # create final structure for each file
    all_final_structure = []
    for k,v in combined.items():
        final_structure = {"sentences": [], 
                           "ner": [], 
                           "relations": [], 
                           "doc_key": k}
        text = v['text']
        sentences = list(nlp(text).sents)
        for sentence in sentences:
            sent_list = []
            for token in sentence:
                if len(set(token.text)) == 1 and set(token.text).pop() == '\n': # if a token consists of linebreaks only
                    continue
                sent_list.append(token.text)
            if len(sent_list):
                final_structure['sentences'].append(sent_list)
        for sent_i in all_matching_token_info_per_sent[k].keys():
            types_sent_list = []
            for (start, end), (token_index, token_type) in all_matching_token_info_per_sent[k][sent_i].items():
                types_sent_list.append([token_index, token_index, token_type])
            if types_sent_list:    
                final_structure['ner'].append(types_sent_list)
        for sent_i in all_matching_relation_info[k].keys():
            relations_sent_list = []
            for token_index, token_index, related_token_index, related_token_index, relation in all_matching_relation_info[k][sent_i]:
                relations_sent_list.append([token_index, token_index, related_token_index, related_token_index, relation])
            if relations_sent_list:
                final_structure['relations'].append(relations_sent_list)
        all_final_structure.append(final_structure)


    
    filename = split + ".json"
    filepath = os.path.join(output_dir, filename)

    with open(filepath, 'w', encoding='utf-8') as file:
            for final_structure in all_final_structure:
                data=json.dumps(final_structure) 
                file.write(data)
                file.write("\n")
    print(f"{split} set is created")



""" statistics part
ctr_all_annotations = 0
ctr_all_match = 0
ctr_all_half_match = 0
ctr_all_missing_match = 0
start_same_end_missing = []
start_missing_end_same = []
for filename, annotated_doc_spans in all_annotated_doc_spans.items():
    print(filename)
    doc_spans = all_doc_spans[filename]
    #print("doc_spans: ", doc_spans)
    #print("annotated_doc_spans: ", annotated_doc_spans)
    ctr_match = 0
    ctr_half_match = 0
    for annotated_doc_span in annotated_doc_spans:
        annotated_start, annotated_end = annotated_doc_span
        for doc_span in doc_spans:
            token, start, end, _ = doc_span
            if annotated_start==start:
                if annotated_end==end:
                    ctr_match+=1
                else:
                    ctr_half_match+=1
                    tokenized_version = combined[filename]['text'][start:end]
                    annotated_token = combined[filename]['text'][annotated_start:annotated_end]
                    start_same_end_missing.append((filename, start, end, tokenized_version, annotated_start, annotated_end, annotated_token))
            else:
                if annotated_end==end:
                    ctr_half_match+=1
                    tokenized_version = combined[filename]['text'][start:end]
                    annotated_token = combined[filename]['text'][annotated_start:annotated_end]
                    start_missing_end_same.append((filename, start, end, tokenized_version, annotated_start, annotated_end, annotated_token))
                else:
                    continue
    print("# of annotated spans: ", len(annotated_doc_spans))
    print("# of perfect match: ", ctr_match)
    print("# of half match: ", ctr_half_match)
    print("# of missing match: ", len(annotated_doc_spans)-ctr_match-ctr_half_match)
    ctr_all_annotations+=len(annotated_doc_spans)
    ctr_all_match += ctr_match
    ctr_all_half_match += ctr_half_match
    ctr_all_missing_match += (len(annotated_doc_spans)-ctr_match-ctr_half_match)

print("# of all annotated spans: ", ctr_all_annotations)
print("# of all matches: ", ctr_all_match)
print("# of all half matches: ", ctr_all_half_match)
print("# of all missing matches: ", ctr_all_missing_match)

with open('new_tokenizer_mistakes.txt', 'w') as file:
    file.write("start span is correct, end span is wrong:\n")
    for filename, start, end, tokenized_version, annotated_start, annotated_end, annotated_token in start_same_end_missing:
        line = f"filename: {filename} tokenizer start: {start} tokenizer end: {end} tokenized word: {tokenized_version} original start: {annotated_start} original end: {annotated_end} original word: {annotated_token}"
        file.write(line)
        file.write('\n')
    file.write("start span is wrong, end span is correct:\n")
    for filename, start, end, tokenized_version, annotated_start, annotated_end, annotated_token in start_missing_end_same:
        line = f"filename: {filename} tokenizer start: {start} tokenizer end: {end} tokenized word: {tokenized_version} original start: {annotated_start} original end: {annotated_end} original word: {annotated_token}"
        file.write(line)
        file.write('\n')

"""
    