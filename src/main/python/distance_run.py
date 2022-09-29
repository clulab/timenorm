from timenorm_data_provider import TimeDataProvider
from transformers import AutoTokenizer
from distance_utils import *
from torch.utils.data import DataLoader
<<<<<<< Updated upstream

model_name = "roberta-base"
tokenizer = AutoTokenizer.from_pretrained(model_name, use_fast=True)
output_path = f"/xdisk/bethard/kbozler/timenorm/relation-extraction/method2/{model_name}/"
data_provider = TimeDataProvider(corpus_dir="./example-data")
relation_to_extract = 'Sub-Interval'
=======
from datasets import load_from_disk
model_name = "roberta-base"
tokenizer = AutoTokenizer.from_pretrained(model_name, use_fast=True)

dataset_path = "/xdisk/bethard/kbozler/timenorm/tempeval-2013-replicate-dataset-object"
#data_provider = TimeDataProvider(corpus_dir="./tempeval-2013-replicate")

relation_to_extract = 'Sub-Interval'
output_path = f"/xdisk/bethard/kbozler/timenorm/relation-extraction/method2/{relation_to_extract}/{model_name}/"
>>>>>>> Stashed changes
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
validity_dict = {
                'Period': [], 
                'Year': ['Month-Of-Year', 'Season-Of-Year'], 
                'Calendar-Interval': [], 
                'Month-Of-Year': ['Day-Of-Month'], 
                'Day-Of-Month': ['Hour-Of-Day', 'Part-Of-Day'], 
                'Day-Of-Week': ['Hour-Of-Day', 'Part-Of-Day'], 
                'Hour-Of-Day': ['Minute-Of-Hour'], 
                'Minute-Of-Hour': ['Second-Of-Minute'], 
                'Number': [], 
                'Second-Of-Minute': [], 
                'Time-Zone': [], 
                'Part-Of-Day': [], 
                'Season-Of-Year': [], 
                'AMPM-Of-Day': [], 
                'Part-Of-Week': [], 
                'Week-Of-Year': ['Day-Of-Week', 'Part-Of-Week'], 
                'Two-Digit-Year': ['Month-Of-Year', 'Season-Of-Year', 'Week-Of-Year'], 
                'Sum': [], 
                'Difference': [], 
                'Union': [], 
                'Intersection': [], 
                'Every-Nth': [], 
                'This': [], 
                'Last': [], 
                'Next': [], 
                'Before': [], 
                'After': [], 
                'Between': [], 
                'NthFromStart': [],
                'NthFromEnd': [], 
                'Frequency': [], 
                'Modifier': [], 
                'Event': [], 
                'Quarter-Of-Year': [], 
                'PreAnnotation': [], 
                'NotNormalizable': [], 
                }



num_labels = len(distances)
<<<<<<< Updated upstream
num_epochs = 3
learning_rate = 2e-5
batch_size = 32

tokenized_dataset = data_provider.read_data_to_distance_format(tokenizer, relation_to_extract, distances)

print("dataset is tokenized and provided")
train_dataloader, eval_dataloader, test_dataloader = load_data(tokenized_dataset, batch_size)
print("dataset is loaded with dataloader")
model = RobertaForTokenClassificationCustom.from_pretrained(pretrained_model_name_or_path='roberta-base', 
                                                                num_labels=num_labels)

setup_results_to_write = {}
model, results_to_write, list_train_loss, list_eval_loss = train_eval_loop(model=model,
                                                                           train_dataloader=train_dataloader,
                                                                           eval_dataloader=eval_dataloader,
                                                                           num_epochs=num_epochs,
                                                                           learning_rate=learning_rate,
                                                                           label_list=distances,
                                                                           validity_dict=validity_dict)  
        
setup_results_to_write[f"num_epochs:{num_epochs}-learning_rate:{learning_rate}-batch_size:{batch_size}"] = results_to_write

setup_output_path = os.path.join(output_path, f"{num_epochs}epochs", f"{learning_rate}learning_rate", f"{batch_size}batch_size")
best_model_step1.save_pretrained(setup_output_path)

test(setup_output_path, os.path.join(data_provider.corpus_dir, "Test"), os.path.join(setup_output_path, 'predictions'))

write_results(setup_output_path, setup_results_to_write)

create_loss_figures(os.path.join(setup_output_path, 'training_loss.png'), list_train_loss, 'training loss over batches - step1')
create_loss_figures(os.path.join(setup_output_path, 'eval_loss.png'), list_eval_loss, 'eval loss over batches - step1')
=======
list_num_epochs = [3, 4]
list_learning_rate = [2e-5,5e-5]
batch_size = 32

#tokenized_dataset = data_provider.read_data_to_distance_format(tokenizer, relation_to_extract, distances)
#tokenized_dataset.save_to_disk(dataset_path)
tokenized_dataset = load_from_disk(dataset_path)

print("tokenized dataset is loaded")
train_dataloader, eval_dataloader, test_dataloader = load_data(tokenized_dataset, batch_size)
print("dataset is loaded with dataloader")

model = RobertaForTokenClassificationCustom.from_pretrained(pretrained_model_name_or_path='roberta-base', 
                                                                num_labels=num_labels)

for num_epochs in list_num_epochs:
    for learning_rate in list_learning_rate:
        setup_results_to_write = {}
        model, results_to_write, list_train_loss, list_eval_loss = train_eval_loop(model=model,
                                                                                   train_dataloader=train_dataloader,
                                                                                   eval_dataloader=eval_dataloader,
                                                                                   num_epochs=num_epochs,
                                                                                   learning_rate=learning_rate,
                                                                                   label_list=distances,
                                                                                   validity_dict=validity_dict)  
                
        setup_results_to_write[f"num_epochs:{num_epochs}-learning_rate:{learning_rate}-batch_size:{batch_size}"] = results_to_write
        setup_output_path = os.path.join(output_path, f"{num_epochs}_epochs-{learning_rate}_learning_rate-{batch_size}_batch_size")
        try:
            os.makedirs(setup_output_path)
        except FileExistsError:
            pass
        write_results(setup_output_path, setup_results_to_write)
        model.save_pretrained(setup_output_path)

#test(setup_output_path, os.path.join(data_provider.corpus_dir, "Test"), os.path.join(setup_output_path, 'predictions'))



#create_loss_figures(os.path.join(setup_output_path, 'training_loss.png'), list_train_loss, 'training loss over batches - step1')
#create_loss_figures(os.path.join(setup_output_path, 'eval_loss.png'), list_eval_loss, 'eval loss over batches - step1')
>>>>>>> Stashed changes



