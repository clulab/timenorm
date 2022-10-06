from timenorm_data_provider import TimeDataProvider
from transformers import AutoTokenizer
from datasets import load_from_disk

model_name = "roberta-base"
tokenizer = AutoTokenizer.from_pretrained(model_name, use_fast=True)
data_provider = TimeDataProvider(corpus_dir="/Users/bulut/timenorm-garage/tempeval-2013-replicate")

relations = ['Sub-Interval', 'Interval', 'Repeating-Interval', 'AMPM-Of-Day', 'Period', 'Number', 'Modifier', 'Every']
labels = ["left", "right", "doctime", "None"]


for relation_to_extract in relations:
    print(relation_to_extract)
    dataset_path = f"/Users/bulut/timenorm-garage/lrd/tempeval-2013-replicate-dataset-object/{relation_to_extract}"
    tokenized_dataset = data_provider.read_data_to_lrd_format(tokenizer, relation_to_extract, labels)
    tokenized_dataset.save_to_disk(dataset_path)
    tokenized_dataset = load_from_disk(dataset_path)
    
