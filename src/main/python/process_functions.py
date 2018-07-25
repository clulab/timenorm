import os
import read_files as read
import numpy as np
import preprocess_functions as process

def prob2classes_multiclasses( prediction):
    if prediction.shape[-1] > 1:
        return prediction.argmax(axis=-1)

def prob2classes_multiclasses_multioutput( prediction):
    output = list()
    for single_predic in prediction:
        if single_predic.shape[-1] > 1:
            output.append(single_predic.argmax(axis=-1))
    return output

def pro2classes_binaryclass(prediction):
    if prediction.shape[-1] <= 1:
        return (prediction > 0.5).astype('int32')

def make_prediction_function_multiclass(x_data,model,output_path):
    y_predict = model.predict(x_data,batch_size=32)
    if len(y_predict)>=2:
        classes = prob2classes_multiclasses_multioutput(y_predict)
    else:
        classes = prob2classes_multiclasses(y_predict)

    if not os.path.exists(output_path):
        os.makedirs(output_path)

    return classes,y_predict

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

def found_location_with_constraint(output):
    """
    :param output: the prediction sequences
    :return: a list of sentences with the span and tag identified
    """
    instance = list()
    instan_index = 0
    for instan in output:
        loc = list()
        for iter in range(len(instan)):
            #if not instan[iter] ==0 and iter <= instance_length[instan_index]-1:   #### with instance_length set
            if not instan[iter] == 0 :  #### without instance_length set
                loc.append([iter,instan[iter]])
        instance.append(loc)
        instan_index +=1
    return instance

def loc2span(loc,probs,post_process = False):
    if post_process == True:
        span_list = list()
        loc_sen_index = 0
        for loc_sen in loc:
            span =list()
            len_loc_sen = len(loc_sen)
            if len_loc_sen <1:
                span.append([])
            else:
                current_location = 0
                while current_location < len_loc_sen:
                    [posi,label] = loc_sen[current_location]
                    n_step_forward = 0
                    prob_init = 0
                    while [posi+n_step_forward, label] in loc_sen:
                        n_step_forward +=1
                        prob_init += probs[loc_sen_index][posi + n_step_forward - 1][label]
                        if not [posi+n_step_forward, label]in loc_sen:
                            span.append([posi,posi+n_step_forward-1,label,prob_init])
                            current_location += n_step_forward
            span_list.append(span)
            loc_sen_index+=1
    else:
        span_list = list()
        for loc_sen in loc:
            span =list()
            len_loc_sen = len(loc_sen)
            if len_loc_sen <1:
                span.append([])
            else:
                current_location = 0
                while current_location < len_loc_sen:
                    [posi,label] = loc_sen[current_location]
                    n_step_forward = 0
                    while [posi+n_step_forward, label] in loc_sen:
                        n_step_forward +=1
                        if not [posi+n_step_forward, label]in loc_sen:
                            span.append([posi,posi+n_step_forward-1,label])
                            current_location += n_step_forward
            span_list.append(span)

    return span_list

def get_gold_dict(tag_file):
    tag_dict = {}
    for sent_tag in tag_file:
        for start, tag in sent_tag:
            print(start,tag)
            tag_dict[start] =tag
    return tag_dict

def get_counts(tag_dict,type):
    count = 0.0
    for start, tag in tag_dict.items():
        if type =="gold":
            count+=float(len(tag[2:]))
        elif type =="pred":
            count += float(len(tag[1:]))
    return count

def calculate_score(gold,pred):
    gold_count= get_counts(gold,"gold")
    pred_count = get_counts(pred,"pred")
    true_count = 0
    for start, tag in pred.items():
        if gold.has_key(start):
            gold_tag = gold[start][2:]
            for tag_pre in tag[1:]:
                if gold[start][0] == tag_pre[0]:
                    if tag_pre[1] in gold_tag:
                        true_count +=1.0
    prec = true_count/pred_count
    reca = true_count/gold_count
    f1 = 2*prec*reca/(prec+reca+1e-10)
    return [gold_count,pred_count,true_count,f1]

def metrics(true_count,pred_count,gold_count):
    precision = true_count / pred_count
    recall = true_count / gold_count
    f1 = 2 * precision * recall / (precision + recall+1e-10)
    print("presion: ", precision, "recall: ", recall, "F1 score: ", f1)



def evaluate(xml_path,output_pred_path,raw_data_path,doc_list,output_format):
    gold_count = 0
    pred_count = 0
    true_count = 0
    for file_id in range(len(doc_list)):
        if os.path.exists(os.path.join(xml_path, doc_list[file_id], doc_list[file_id] + "_tag")):
            gold_tag_dict = get_gold_dict(read.readfrom_json(os.path.join(xml_path, doc_list[file_id], doc_list[file_id] + "_tag")))
            output_path = os.path.join(output_pred_path, doc_list[file_id], doc_list[file_id] + output_format)
            raw_text_path = os.path.join(raw_data_path, doc_list[file_id], doc_list[file_id])
            pre_tag_dict = process.extract_xmltag_anafora_pred(output_path, read.readfrom_txt(raw_text_path))
            scores = calculate_score(gold_tag_dict, pre_tag_dict)
            gold_count += scores[0]
            pred_count += scores[1]
            true_count += scores[2]
            metrics(true_count, pred_count, gold_count)







