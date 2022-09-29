from timenorm_data_provider import TimeDataProvider
from transformers import AutoTokenizer
from distance_utils import *
from datasets import load_from_disk

model_name = "roberta-base"
tokenizer = AutoTokenizer.from_pretrained(model_name, use_fast=True)
data_provider = TimeDataProvider(corpus_dir="/Users/bulut/timenorm-garage/tempeval-2013-replicate")

relations = ['Sub-Interval', 'Interval', 'Repeating-Interval', 'AMPM-Of-Day', 'Period', 'Number', 'Modifier', 'Every']
distances_per_relation = {'Sub-Interval': ['-6', '-5', '-4', '-3', '-2', '-1', '1', '2', '3', '4', 'None'],
                          'Interval': ['-5', '-4', '-3', '-2', '-1', 'None'],
                          'Repeating-Interval': ['-2', '-1', '0', '1', 'None'],
                          'AMPM-Of-Day': ['-3', '-1', 'None'],
                          'Period': ['-3', '-2', '-1', '1', 'None'],
                          'Number': ['0', '1', '2', '3', '4', '5', 'None'],
                          'Modifier': ['1', '2', '3', 'None'],
                          'Every': ['-6', '-5', '-4', '-3', '-2', '-1', 'None']
                         }


for relation_to_extract in relations:
    print(relation_to_extract)
    distances = distances_per_relation[relation_to_extract]
    dataset_path = f"/Users/bulut/timenorm-garage/tempeval-2013-replicate-dataset-object/{relation_to_extract}"
    tokenized_dataset = data_provider.read_data_to_distance_format(tokenizer, relation_to_extract, distances)
    tokenized_dataset.save_to_disk(dataset_path)
    tokenized_dataset = load_from_disk(dataset_path)
    
