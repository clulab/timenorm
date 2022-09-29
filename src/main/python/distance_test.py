from timenorm_data_provider import TimeDataProvider
from transformers import AutoTokenizer
from distance_utils import *
from torch.utils.data import DataLoader
from datasets import load_from_disk
model_name = "roberta-base"
tokenizer = AutoTokenizer.from_pretrained(model_name, use_fast=True)

data_provider = TimeDataProvider(corpus_dir="/Users/bulut/timenorm-garage/tempeval-2013-replicate")
test_input_path = os.path.join(data_provider.corpus_dir, "Test")
relation_to_extract = 'Sub-Interval'
model_path = f"/Users/bulut/timenorm-garage/relation-extraction/method2/{relation_to_extract}/model/"

test_output_path = f"/Users/bulut/timenorm-garage/relation-extraction/method2/{relation_to_extract}/{model_name}-test-output/"

types, _, _ = data_provider.get_time_type_lists('./utilTextFiles')
index_to_type = {i: l for i, l in enumerate(types)}
distances_per_relation = {'Sub-Interval': ['-6', '-5', '-4', '-3', '-2', '-1', '1', '2', '3', '4', 'None'],
                          'Interval': ['-5', '-4', '-3', '-2', '-1', 'None'],
                          'Repeating-Interval': ['-2', '-1', '0', '1', 'None'],
                          'AMPM-Of-Day': ['-3', '-1', 'None'],
                          'Period': ['-3', '-2', '-1', '1', 'None'],
                          'Number': ['0', '1', '2', '3', '4', '5', 'None'],
                          'Modifier': ['1', '2', '3', 'None']
                         }
distances = distances_per_relation[relation_to_extract]
index_to_distance = {i: l for i, l in enumerate(distances)}



model = RobertaForTokenClassificationCustom.from_pretrained(model_path)


test(model, tokenizer, test_input_path, test_output_path, None, index_to_distance, relation_to_extract)






