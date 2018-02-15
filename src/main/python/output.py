import read_files as read
import os
import process_functions as output
import numpy as np
from keras.models import load_model
import argparse


def span2xmlfiles(data_spans,file_name_simple):
    import anafora
    data = anafora.AnaforaData()
    id = 0
    for data_span in data_spans:
        e = anafora.AnaforaEntity()
        e.spans = ((int(data_span[0]), int(data_span[1])+1),)
        e.type = data_span[2]
        e.id = str(id)+"@e@" + file_name_simple
        data.annotations.append(e)
        id+=1
    data.indent()
    return data

def generate_output_multiclass(model,input,gold,doc_list_sub, processed_path,output_pred_path,pred =True,data_folder = "",format_abbre = ".TimeNorm.system.completed.xml"):
    non_operator = read.textfile2list("data/config_data/label/non-operator.txt")
    operator = read.textfile2list("data/config_data/label/operator.txt")
    labels_index = [non_operator,operator,operator]

    if pred == True:
        classes,probs  = output.make_prediction_function_multiclass(input, model, output_pred_path, data_folder) #,,x_unic_onehot
    else:
        classes= np.load(output_pred_path+"/y_predict_classes"+data_folder+".npy")
        probs = read.readfrom_pickle(output_pred_path + "/y_predict_proba"+data_folder)

    spans = list()
    int2labels = list()
    for index in range(len(classes)):

        class_loc = output.found_location_with_constraint(classes[index])
        span = output.loc2span(class_loc, probs[index],post_process = False)
        spans.append(span)

        one_hot = read.counterList2Dict(list(enumerate(labels_index[index], 1)))
        one_hot = {y: x for x, y in one_hot.iteritems()}
        int2label = dict((int, char) for char, int in one_hot.items())
        int2labels.append(int2label)

    n_marks =3
    sent_index = 0

    for data_id in range(0,len(doc_list_sub)):
        sent_spans = read.readfrom_json(os.path.join(processed_path,doc_list_sub[data_id],doc_list_sub[data_id]+"_sent"))
        data_span = list()
        for sent_span in sent_spans:
            for index in range(len(classes)):
                span_list = spans[index][sent_index]
                if len(span_list[0]) <1:
                    pass
                else:
                    for [posi_start,posi_end,label] in span_list:
                        data_span.append([posi_start-n_marks+sent_span[1],posi_end-n_marks+ sent_span[1],int2labels[index][label]])
            sent_index += 1
        data = span2xmlfiles(data_span,doc_list_sub[data_id])
        output_path = os.path.join(output_pred_path,doc_list_sub[data_id],doc_list_sub[data_id])
        read.create_folder(output_path)
        data.to_file(output_path+format_abbre)
    del classes,probs,input


def main(model_path,doc_list,raw_data_path, preocessed_path, output_pred_path,output_format,evaluate = True):
    model = load_model(model_path)
    file_n = len(doc_list)
    folder_n = np.divide(file_n,20)
    folder = map(lambda x: int(x), np.linspace(0, file_n, folder_n + 1))
    input_path = "/".join(preocessed_path.split('/')[:-1])
    if file_n>20:
        for version in range(6,folder_n):
            start = folder[version]
            end = folder[version + 1]
            doc_list_sub = doc_list[start:end]
            input = read.load_hdf5(input_path+"/train_input"+str(version),["char","pos","unic"])
            gold = None
            generate_output_multiclass(model, input,gold, doc_list_sub, preocessed_path,output_pred_path,data_folder = str(version),format_abbre =output_format)
    else:
        start = 0
        end = file_n
        doc_list_sub = doc_list[start:end]
        input = read.load_hdf5(input_path+"/train_input", ["char", "pos", "unic"])
        gold = None
        generate_output_multiclass(model, input,gold,doc_list_sub,preocessed_path, output_pred_path,format_abbre =output_format)

    if evaluate=="true":
        output.evaluate(preocessed_path,output_pred_path,raw_data_path,doc_list,output_format)



if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Process features and output encoding for time identification task.')
    parser.add_argument('--raw',
                        help='raw data path',required=True)
    parser.add_argument('--model',
                        help='specify the model path',default="")
    parser.add_argument('--preprocessed_path',
                        help='specify the preprocessed path',default="")

    parser.add_argument('--out',
                        help='output path for all preprocessed files',required=True)

    parser.add_argument('--format',
                        help='output path for all preprocessed files',default=".TimeNorm.gold.completed.xml")

    parser.add_argument('--evaluate',
                        help='Whether requried to be evaluated',default="false")

    parser.add_argument('--mode',
                        help='Whether requried to calculate the scores',default="false")

    args = parser.parse_args()
    raw_data_path = args.raw
    preprocessed_path = args.preocessed_path
    model_path = args.model
    output_pred_path = args.out
    output_format = args.format
    evaluate = args.evaluate    # true
    mode = args.mode         # pred


    # model_path = "data/config_data/model/char-3softmax-extra/weights-improvement-685.hdf5"
    # raw_data_path = "data/THYMEColonFinal/Dev"
    # preocessed_path = "data/Processed_THYMEColonFinal1/Dev"
    # output_pred_path = "data/Cancer_Ident1"
    # output_format = ".TimeNorm.system.completed.xml"

    if __name__ == "__main__":
        doc_list = []
        for doc in os.listdir(preprocessed_path):
            if not doc.endswith(".txt") and not doc.endswith(".npy"):
                doc_list.append(doc)
        main(model_path,doc_list,raw_data_path, preprocessed_path, output_pred_path,output_format,pred = mode,evaluate = evaluate)
