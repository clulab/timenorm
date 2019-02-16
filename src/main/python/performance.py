import numpy as np
np.random.seed(20170915)
import os
import h5py
from collections import OrderedDict
from collections import Iterable
import csv


from keras.models import load_model


def load_input(filename):
    data = list()
    with h5py.File( 'data/' + filename + '.hdf5', 'r') as hf:
        print("List of arrays in this file: \n", hf.keys())
        for key in hf.keys():
            x = hf.get(key)
            x_data = np.array(x)
            print(x_data.shape)
            del x
            data.append(x_data)

    return data


def load_hdf5(filename,labels):
    data = list()
    with h5py.File(filename + '.hdf5', 'r') as hf:
        print("List of datum in this file: ", hf.keys())
        for label in labels:
            x = hf.get(label)
            x_data = np.array(x)
            del x
            print("The shape of datum "+ label +": ",x_data.shape)
            data.append(x_data)
    return data

def prob2classes_multiclasses_multioutput( prediction):
    output = list()
    for single_predic in prediction:
        if single_predic.shape[-1] > 1:
            output.append(single_predic.argmax(axis=-1))
    return output

def hot_vectors2class_index (labels):
    examples = list()
    for instance in labels:
        label_index = list()
        for label in instance:
            if 1 in list(label):
                k = list(label).index(1)
                label_index.append(k)
            else:
                label_index.append(0)
        examples.append(label_index)
    return examples

def found_location_with_constraint(k):
    instance = list()
    instan_index = 0
    for instan in k:
        loc = list()
        for iter in range(len(instan)):
            #if not instan[iter] ==0 and iter <= instance_length[instan_index]-1:   #### with instance_length set
            if not instan[iter] == 0 :  #### without instance_length set
                loc.append([iter,instan[iter]])
        instance.append(loc)
        instan_index +=1
    return instance

def location2span(loc):
    span_list = list()
    for loc_sen in loc:
        span = dict()
        len_loc_sen = len(loc_sen)
        if len_loc_sen < 1:
            None
        else:
            current_location = 0
            while current_location < len_loc_sen:
                [posi, label] = loc_sen[current_location]
                n_step_forward = 0
                while [posi + n_step_forward, label] in loc_sen:
                    n_step_forward += 1
                    if not [posi + n_step_forward, label] in loc_sen:
                        span[posi]= [posi+n_step_forward-1, label]
                        current_location += n_step_forward
        span_list.append(span)
    # print span_list
    return span_list

def socres(match,pred,gold):
    precision = float(match)/float(pred) if pred >0 else 0.0
    recall = float(match)/float(gold) if gold >0 else 0.0
    f1 = 2*precision*recall/(precision + recall) if precision + recall >0 else 0.0
    return precision,recall,f1


def calculate_precision_multi_class(result,gold_loc):
    pred = 0.0
    gold = 0.0
    match = 0.0
    predictions = location2span(result)
    golds = location2span(gold_loc)
    for iter in range(len(result)):
        pred += float(len(predictions[iter]))
        gold += float(len(golds[iter]))
        for key in sorted(predictions[iter].keys()):
            if key in golds[iter]:
                if predictions[iter][key][0] ==golds[iter][key][0] and predictions[iter][key][1] ==golds[iter][key][1]:
                    match += float(1)
    # precision = float(match)/float(pred) if pred >0 else 0.0
    # recall = float(match)/float(gold) if gold >0 else 0.0
    # f1 = 2*precision*recall/(precision + recall) if precision + recall >0 else 0.0
    precision, recall, f1 = socres(match, pred, gold)
    return pred,match,gold,precision,recall,f1

def span_level_performace(storage,val_data,gold_locs,max_epoch=800,filename="", append=False):
    writer = None
    keys = None
    append_header = True
    if append:
        if os.path.exists(filename):
            with open(filename) as f:
                append_header = bool(len(f.readline()))
        csv_file = open(filename, 'a')
    else:
        csv_file = open(filename, 'w')

    for epoch in range(50,800):
        epoch_string = '%02d' % epoch
        filepath = storage +"/weights-improvement-%s.hdf5" % epoch_string
        print(filepath)
        model = load_model(filepath)
        y_predict = model.predict(val_data, batch_size=400)
        classes = prob2classes_multiclasses_multioutput(y_predict)
        matchs = list()
        taggeds = list()
        precisions = list()
        recalls = list()
        f1s = list()
        golds = list()
        for index in range(3):
            class_loc = found_location_with_constraint(classes[index])

            gold_loc = gold_locs[index]
            (n_pre, n_match, n_gold, precision, recall, f1) = calculate_precision_multi_class(class_loc, gold_loc)
            matchs.append(n_match)
            taggeds.append(n_pre)
            precisions.append(precision)
            recalls.append(recall)
            f1s.append(f1)
            golds.append(n_gold)
        precision_overall, recall_overall, f1_overall = socres(sum(matchs), sum(taggeds), sum(golds))
        precision_non_im, recall_non_im, f1_non_im = socres(matchs[0] + matchs[2], taggeds[0] + taggeds[2],
                                                            golds[0] + golds[2])
        performance = {"match1": matchs[0], "tagged1": taggeds[0], "precision1": precisions[0], "recall1": recalls[0],
                       "f1_1": f1s[0],
                       "match2": matchs[1], "tagged2": taggeds[1], "precision2": precisions[1], "recall2": recalls[1],
                       "f1_2": f1s[1],
                       "match3": matchs[2], "tagged3": taggeds[2], "precision3": precisions[2], "recall3": recalls[2],
                       "f1_3": f1s[2],
                       "precision_all": precision_overall, "recall_all": recall_overall, "f1_all": f1_overall,
                       "precision_non_im": precision_non_im, "recall_non_im": recall_non_im, "f1_non_im": f1_non_im, }

        def handle_value(k):
            is_zero_dim_ndarray = isinstance(k, np.ndarray) and k.ndim == 0
            if isinstance(k, Iterable) and not is_zero_dim_ndarray:
                return '"[%s]"' % (', '.join(map(str, k)))
            else:
                return k

        if not writer:
            keys = sorted(performance.keys())
            writer = csv.DictWriter(csv_file,
                                    fieldnames=['epoch'] + keys)
            if append_header:
                writer.writeheader()

        row_dict = OrderedDict({'epoch': epoch})
        row_dict.update((key, handle_value(performance[key])) for key in keys)
        writer.writerow(row_dict)
        csv_file.flush()
    csv_file.close()



######################### Notes ###################################################################################
#### You need to change the $storage,  $filename   $file_path  for the model, csv file name, path of input datasets

def __main__():
    storage = "/xdisk/bethard/domain_adaptation/flair_news_dense256"  ##### The repo for storing the models

    filename="training_real_performance.csv" ##### The csv output file


    file_path = "/extra/dongfangxu9/domain_adaptation/data/data_mixed"   ### The repo you save your datasets

    char_x_cv= load_hdf5(file_path + "/cvnews_train_input_flair", ["char"])[0]
    cv_y_interval,cv_y_operator_ex,cv_y_operator_im = load_hdf5(file_path + "/cvnews_train_target", ["interval_softmax","explicit_operator","implicit_operator"])



    labels = [cv_y_interval, cv_y_operator_ex, cv_y_operator_im]
    gold_locs = list()
    for index in range(3):
        gold = hot_vectors2class_index(labels[index])
        gold_loc = found_location_with_constraint(gold)
        gold_locs.append(gold_loc)


    span_level_performace(storage,char_x_cv,gold_locs,filename = filename)
