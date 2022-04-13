from datasets import load_dataset
from torchmetrics import Precision, Recall, F1Score
from transformers import AutoTokenizer, AutoModelForTokenClassification, AutoConfig
from torch.utils.data import DataLoader
from transformers import get_scheduler
import torch
import numpy as np
from tqdm.auto import tqdm
import os
import math
model_name = "distilbert-base-uncased"
dataset = load_dataset("conll2003")
tokenizer = AutoTokenizer.from_pretrained(pretrained_model_name_or_path=model_name)

output_path_step1 = f"/xdisk/bethard/kbozler/directed-research-1/best-output/{model_name}/step1/"
output_path_step2 = f"/xdisk/bethard/kbozler/directed-research-1/best-output/{model_name}/step2/"
output_path_step3 = f"/xdisk/bethard/kbozler/directed-research-1/best-output/{model_name}/step3/"

list_num_epochs = [3]
list_learning_rate = [2e-5, 5e-5]
train_batch_size = 32
eval_batch_size = 32
test_batch_size = 32

device = torch.device("cuda") if torch.cuda.is_available() else torch.device("cpu") # set it to cpu during debugging

def load_data(task_name):
    tokenized_dataset = prepare_dataset(dataset, task_name)
    print("dataset is tokenized")
    
    shuffled_train_dataset = tokenized_dataset["train"].shuffle(seed=42).select(range(len(tokenized_dataset["train"])))
    shuffled_eval_dataset = tokenized_dataset["validation"].shuffle(seed=42).select(range(len(tokenized_dataset["validation"])))
    shuffled_test_dataset = tokenized_dataset["test"].shuffle(seed=42).select(range(len(tokenized_dataset["test"])))
    '''
    shuffled_train_dataset = tokenized_dataset["train"].shuffle(seed=42).select(range(100))
    shuffled_eval_dataset = tokenized_dataset["validation"].shuffle(seed=42).select(range(100))
    shuffled_test_dataset = tokenized_dataset["test"].shuffle(seed=42).select(range(100))
    '''
    print("dataset is shuffled and splitted")
    
    train_dataloader = DataLoader(shuffled_train_dataset, batch_size=train_batch_size)
    eval_dataloader = DataLoader(shuffled_eval_dataset, batch_size=eval_batch_size)
    test_dataloader = DataLoader(shuffled_test_dataset, batch_size=test_batch_size)
    
    return train_dataloader, eval_dataloader, test_dataloader

def tokenize_and_align_labels(samples, task_name):
    tokenized_samples = tokenizer(samples["tokens"], padding="max_length", truncation=True, is_split_into_words=True) # since the tokens are already splitted, we need to
                                                                                                # set is_split_into_words True
    aligned_labels = [] 
    unaligned_labels = samples[f"{task_name}_tags"] # examples[f"{task}_tags"] returns list of sample labels 
                                                # for a list of samples before tokenization (therefore unaligned)
    for sample_index, sample_labels in enumerate(unaligned_labels): 
                                                          # sample_labels is basically list of labels for a single sample in the dataset
                                                          # sample_index represents index for a sample in a list of samples
        
        aligned_sample_word_ids = tokenized_samples.word_ids(batch_index=sample_index) # word_ids here represents what each tokenized token 
                                                            # belongs to which untokenized token originally 
                                                            # batch_index here is to obtain ith sample's word_ids after tokenization
        aligned_sample_labels = [] # list of label ids for a single sample in the dataset AFTER the tokenization
        for word_id in aligned_sample_word_ids:           
            if word_id is None:         # special tokens like [CLS] and [SEP] have a word id that is None. 
                aligned_sample_labels.append(-100)    # their label should be set to -100 so they are automatically ignored in the loss function in pytorch.
            else:                           # non special tokens have a word id that represents its untokenized version   
                aligned_sample_labels.append(sample_labels[word_id]) # its label should be corresponding label of its untokenized version

        aligned_labels.append(aligned_sample_labels) # aligned_labels becomes list of lists

    tokenized_samples["labels"] = aligned_labels # change the original labels with aligned versions
    return tokenized_samples

def prepare_dataset(dataset, task_name):
    tokenized_dataset = dataset.map(tokenize_and_align_labels, batched=True, fn_kwargs={"task_name":task_name})
    for col in tokenized_dataset['train'].features.keys():
        if col != 'input_ids' and col != 'labels':
            tokenized_dataset = tokenized_dataset.remove_columns([col])
    tokenized_dataset.set_format("torch")
    return tokenized_dataset

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
    return results

def train_eval_loop(train_dataloader, eval_dataloader, num_epochs, learning_rate, task_name, step3):
    num_training_steps = num_epochs * len(train_dataloader)
    num_eval_steps = num_epochs * len(eval_dataloader)
    progress_bar_train = tqdm(range(num_training_steps))
    progress_bar_eval = tqdm(range(num_eval_steps))
    results_to_write = []
    list_train_loss = []
    list_eval_loss = []
    
    
    
    label_list = dataset["train"].features[f"{task_name}_tags"].feature.names
    num_labels = len(label_list)
    if step3:
        config = AutoConfig.from_pretrained(pretrained_model_name_or_path=output_path_step1)
        config.num_labels = num_labels
        model = AutoModelForTokenClassification.from_config(config)
    else:
        model = AutoModelForTokenClassification.from_pretrained(pretrained_model_name_or_path=model_name, 
                                                                num_labels=num_labels)

    model.to(device)

    optimizer = torch.optim.AdamW(model.parameters(), lr=learning_rate)
    scheduler = get_scheduler(name="linear", 
                                 optimizer=optimizer, 
                                 num_warmup_steps=0, 
                                 num_training_steps=num_training_steps)
    
    for epoch in range(num_epochs):
        model.train()
        for batch in train_dataloader:
            batch = {k: v.to(device) for k, v in batch.items()}
            outputs = model(**batch)
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
            batch = {k: v.to(device) for k, v in batch.items()}
            with torch.no_grad():
                outputs = model(**batch)
            logits = outputs.logits
            batch_predictions = torch.argmax(logits, dim=-1)
            eval_predictions.append(batch_predictions)
            eval_labels.append(batch["labels"])
            progress_bar_eval.update(1)
            list_eval_loss.append(outputs.loss.item())
        
        predictions = torch.cat(tuple(eval_predictions), 0)
        labels = torch.cat(tuple(eval_labels), 0)
        results = compute_metrics(predictions, labels, label_list)
        print(f"epoch {epoch+1}:", results)
        results_line = "epoch: " + str(epoch+1) + " precision_micro: " + str(results["precision_micro"]) + " precision_macro: " + str(results["precision_macro"]) + " recall_micro: " + str(results["recall_micro"]) + " recall_macro: " + str(results["recall_macro"]) + " f1_micro: " + str(results["f1_micro"]) + " f1_macro: " + str(results["f1_macro"])
        results_to_write.append(results_line)
    return model, results_to_write, results["f1_micro"], list_train_loss, list_eval_loss

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

def test(model, test_dataloader, task_name):
    num_test_steps = len(test_dataloader)
    label_list = dataset["train"].features[f"{task_name}_tags"].feature.names
    progress_bar_test = tqdm(range(num_test_steps))
    test_predictions = []
    test_labels = []
    for batch in test_dataloader:
        batch = {k: v.to(device) for k, v in batch.items()}
        with torch.no_grad():
            outputs = model(**batch)
        logits = outputs.logits
        batch_predictions = torch.argmax(logits, dim=-1)
        test_predictions.append(batch_predictions)
        test_labels.append(batch["labels"])
        progress_bar_test.update(1)
    predictions = torch.cat(tuple(test_predictions), 0)
    labels = torch.cat(tuple(test_labels), 0)
    results = compute_metrics(predictions, labels, label_list)
    print(results)

