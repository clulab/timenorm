from timenorm_data_provider import TimeDataProvider
from transformers import AutoTokenizer

tokenizer = AutoTokenizer.from_pretrained("roberta-base", use_fast=True)

data_loader = TimeDataProvider(corpus_dir="./example-data/train")
relation_to_extract = 'Sub-Interval'
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
f = open('./types.txt')
lines = f.readlines()
types = []
for line in lines:
    types.append(line.replace("\n", ""))

inputs, labels = data_loader.read_data_to_distance_format(tokenizer, relation_to_extract, distances, types)