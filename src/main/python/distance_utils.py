from torch.utils.data import DataLoader
from torchmetrics import Precision, Recall, F1Score
from transformers import get_scheduler
import torch
import numpy as np
from tqdm.auto import tqdm
import os
import math
from distance_custom_models import RobertaForTokenClassificationCustom


def load_data(tokenized_dataset, batch_size):
    
    train_dataloader = DataLoader(tokenized_dataset['train'], batch_size=batch_size)
    eval_dataloader = DataLoader(tokenized_dataset['validation'], batch_size=batch_size)
    test_dataloader = DataLoader(tokenized_dataset['test'], batch_size=batch_size)
    
    return train_dataloader, eval_dataloader, test_dataloader

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

def train_eval_loop(model, train_dataloader, eval_dataloader, num_epochs, learning_rate, label_list, validity_dict):
    num_training_steps = num_epochs * len(train_dataloader)
    num_eval_steps = num_epochs * len(eval_dataloader)
    progress_bar_train = tqdm(range(num_training_steps))
    progress_bar_eval = tqdm(range(num_eval_steps))
    results_to_write = []
    list_train_loss = []
    list_eval_loss = []
    device = torch.device("cuda") if torch.cuda.is_available() else torch.device("cpu") # set it to cpu during debugging
    model.to(device)

    optimizer = torch.optim.AdamW(model.parameters(), lr=learning_rate)
    scheduler = get_scheduler(name="linear", 
                                 optimizer=optimizer, 
                                 num_warmup_steps=0, 
                                 num_training_steps=num_training_steps)
    
    for epoch in range(num_epochs):
        model.train()
        for batch in train_dataloader:
            batch = {k: torch.stack(v).to(device) for k, v in batch.items()}
            outputs = model.forward(input_ids=batch['input_ids'], labels_type=batch['labels_type'], labels_distance=batch['labels_distance'], validity_dict=validity_dict)
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
            batch = {k: torch.stack(v).to(device) for k, v in batch.items()}
            with torch.no_grad():
                outputs = model.forward(input_ids=batch['input_ids'], labels_type=batch['labels_type'], labels_distance=batch['labels_distance'], validity_dict=validity_dict)
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
    return model, results_to_write, list_train_loss, list_eval_loss

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