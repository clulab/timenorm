from datasets import load_from_disk
from transformers import AutoTokenizer
import os
model_name = "roberta-base"
tokenizer = AutoTokenizer.from_pretrained(model_name, use_fast=True)
types = []
f = open("/Users/bulut/local-repositories/timenorm/src/main/python/utilTextFiles/types.txt")
lines = f.readlines()
for line in lines:
    types.append(line.replace("\n", ""))
labels = ["left", "right", "doctime", "None"]

index_to_label = {i: l for i, l in enumerate(labels)}
index_to_type = {i: l for i, l in enumerate(types)}
dataset_path = "/Users/bulut/timenorm-garage/lrd/tempeval-2013-replicate-dataset-object/Interval"

tokenized_dataset = load_from_disk(dataset_path)

ctr_if_relation_exist = 0
train = tokenized_dataset['train']
for i in range(len(train)):
    sentence_words = tokenizer.decode(train[i]['input_ids'], skip_special_tokens=True, clean_up_tokenization_spaces=True)
    
    length = train[i]['input_ids'].index(2)
    sentence_types = [index_to_type[index] for index in train[i]['labels_type']][:length]
    sentence_labels = [index_to_label[index] for index in train[i]['labels_location']][:length]
    if set(sentence_labels) == {"None"}:
        continue
    ctr_if_relation_exist += 1
    print(sentence_words)
    for j in range(length):
        word = tokenizer.decode(train[i]['input_ids'][j])
        if sentence_labels[j] != "None":
            sentence_labels[j] += "***"
        print("word: ", word, "type: ", sentence_types[j], "label: ", sentence_labels[j])

print("# of sentences that at least have a relation: ", ctr_if_relation_exist)
print("# of total sentences in the set: ", len(train))