# encoding: utf-8
import preprocess_functions as process
import read_files as read
import os
from nltk.tokenize import sent_tokenize
from nltk.tokenize.util import regexp_span_tokenize
import numpy as np
from collections import defaultdict
from random import randint
import argparse

#############################processing time-ml file into raw data and anofora format ############################################
# import anafora.timeml as timeml
 # timeml._timeml_dir_to_anafora_dir("data/TBAQ-cleaned/","data/Processed",schema_name="TimeML")
#################################################################################################################################

############################  read xml from file  #########################################
# data = anafora.AnaforaData.from_file("ABC19980108.1830.0711/ABC19980108.1830.0711.TimeNorm.gold.completed.xml")
# for annotation in data.annotations:
#     annotation.spans
#     annotation.type
###########################################################################################

def get_xml_dir(read_dirname,file_filters=[],file_format=".TimeNorm.gold.completed.xml",has_root_folder=True):
    '''
    get the directory for whole raw data and xml data, using the same root dir raw_text_dir
    :return:  a list of xml_data directory folder
    '''
    file_dir = list()
    root_folders = list()
    if has_root_folder ==True:
        roots = os.listdir(read_dirname)
        for root in roots:
            root_com =os.path.join(read_dirname,root)
            root_folders += [os.path.join(root_com,f) for f in os.listdir(root_com) if os.path.isdir(os.path.join(root_com, f))]
    else:
        root_folders = [os.path.join(read_dirname, f) for f in os.listdir(read_dirname)]

    file_filters = [file_filter.split('/')[-1] for file_filter in file_filters ]


    for root_folder in root_folders:
        for file in os.listdir(root_folder):
                if file_format in file:
                    file_id = file.replace(file_format,"")
                    if len(file_filters)>0 and file_id in file_filters:
                        file_dir.append(file_id)
                    elif len(file_filters)==0:
                        file_dir.append(file_id)
                    else:
                        pass
    return file_dir

def split_by_sentence(raw_text,char_vocab):
    sent_tokenize_list = sent_tokenize(raw_text)
    sent_tokenize_span_list = process.spans(sent_tokenize_list, raw_text)
    sent_span_list = list()
    max_len = list()
    for sent_tokenize_span in sent_tokenize_span_list:
        sent_spans = list(regexp_span_tokenize(sent_tokenize_span[0], r'\n'))
        for sent_span in sent_spans:
            sent_span = (sent_span[0] + sent_tokenize_span[1], sent_span[1] + sent_tokenize_span[1])
            sent = raw_text[sent_span[0]:sent_span[1]]
            for char in sent:
                char_vocab[char]+=1
            if len(sent) >= 350:
                #print sent
                multi_sent_span, multi_sent_len = process.rule_based_tokenizer(sent, sent_span)
                sent_span_list += multi_sent_span
                max_len += multi_sent_len
                if max(multi_sent_len)>350:
                    print sent
            else:
                sent_span_list.append([sent, sent_span[0], sent_span[1]])
                max_len.append(len(sent))
    return sent_span_list,max_len,char_vocab

        #read.save_in_json(raw_dir_simple[data_id] + "_sent", sent_span_list)
        #max_len.sort(reverse=True)

def xml_tag_in_sentence(sentences,posi_info_dict):
    tag_list = list()
    tag_span = posi_info_dict.keys()
    tag_span = sorted(tag_span, key=int)
    i = 0
    for sent in sentences:
        tag = list()
        if i < len(tag_span):
            if sent[2] < int(tag_span[i]):
                tag_list.append(tag)
            elif sent[1] <= int(tag_span[i]) and sent[2] > int(tag_span[i]):
                while True:
                    tag.append((tag_span[i],posi_info_dict[tag_span[i]]))
                    i = i + 1
                    if i < len(tag_span):
                        if int(tag_span[i]) > sent[2]:
                            tag_list.append(tag)
                            break
                    else:
                        tag_list.append(tag)
                        break
        else:
            tag_list.append(tag)
        #print tag_list
    return tag_list

def get_idx_from_sent(padding_char,sent, word_idx_map, max_l,pad):
    """
    Transforms sentence into a list of indices. Post-Pad with zeroes.
    """
    x = []
    for i in range(pad):
        x.append(word_idx_map[padding_char])
    for word in sent:
        if word in word_idx_map.keys():
            x.append(word_idx_map[word])
        else:
            x.append(word_idx_map["unknown"])
    for i in range(pad):
        x.append(word_idx_map[padding_char])
    while len(x) < max_l+ 2 *pad:
        x.append(0)
    return x

def create_class_weight(n_labels, labels, mu):
    n_softmax = n_labels
    # class_index = hot_vectors2class_index_forweights(labels)
    counts = np.zeros(n_softmax, dtype='int32')
    for softmax_index in labels:
        softmax_index = np.asarray(softmax_index)
        for i in range(n_softmax):
            counts[i] = counts[i] + np.count_nonzero(softmax_index == i)

    labels_dict = read.counterList2Dict(list(enumerate(counts, 0)))

    total = np.sum(labels_dict.values())
    class_weight = dict()

    for key, item in labels_dict.items():
        if not item == 0:
            score = mu * total / float(item)
            class_weight[key] = score if score > 1.0 else 1.0
        else:
            class_weight[key] = 10.0

    return class_weight


def get_sample_weights_multiclass(n_labels, labels, mu1):
    class_weight = create_class_weight(n_labels, labels, mu=mu1)
    # class_index = np.asarray(hot_vectors2class_index_forweights(labels))
    samples_weights = list()
    for instance in labels:
        sample_weights = [class_weight[category] for category in instance]
        samples_weights.append(sample_weights)
    return samples_weights


def document_level_2_sentence_level(file_dir, raw_data_path, preprocessed_path,xml_path,file_format):

    max_len_all=list()

    char_vocab = defaultdict(float)
    pos_vocab = defaultdict(float)
    unicode_vocab = defaultdict(float)

    for data_id in range(0, len(file_dir)):
        raw_text_path = os.path.join(raw_data_path,file_dir[data_id],file_dir[data_id])
        preprocessed_file_path = os.path.join(preprocessed_path,file_dir[data_id],file_dir[data_id])


        raw_text = read.readfrom_txt(raw_text_path)
        raw_text = process.text_normalize(raw_text)
        sent_span_list_file, max_len_file,char_vocab = split_by_sentence(raw_text,char_vocab)

        max_len_all +=max_len_file

        pos_sentences, pos_vocab = process.get_pos_sentence(sent_span_list_file, pos_vocab)
        pos_sentences_character = process.word_pos_2_character_pos(sent_span_list_file, pos_sentences)
        unico_sentences_characte,unicode_vocab = process.get_unicode(sent_span_list_file,unicode_vocab)


        read.savein_json(preprocessed_file_path+"_sent",sent_span_list_file)
        read.savein_json(preprocessed_file_path + "_pos", pos_sentences_character)
        read.savein_json(preprocessed_file_path + "_unicodecategory", unico_sentences_characte)
        if xml_path != None:
            xml_file_path = os.path.join(xml_path, file_dir[data_id], file_dir[data_id] + file_format)
            posi_info_dict = process.extract_xmltag_anafora(xml_file_path, raw_text)
            sent_tag_list_file = xml_tag_in_sentence(sent_span_list_file, posi_info_dict)
            read.savein_json(preprocessed_file_path + "_tag", sent_tag_list_file)



    max_len_all.sort(reverse=True)
    max_len_file_name = "/".join(preprocessed_path.split('/')[:-1])+"/max_len_sent"
    read.savein_json(max_len_file_name, max_len_all)

def features_extraction(raw_data_dir,output_folder,data_folder = "",mode = "train"):
    max_len = 350
    pad = 3
    input_char = list()
    input_pos = list()
    input_unic = list()
    char2int = read.readfrom_json("data/config_data/vocab/char2int")
    pos2int = read.readfrom_json("data/config_data/vocab/pos2int")
    unicode2int = read.readfrom_json("data/config_data/vocab/unicate2int")
    total = 0
    for data_id in range(0, len(raw_data_dir)):
        preprocessed_file_path = os.path.join(preprocessed_path, file_dir[data_id], file_dir[data_id])
        sent_span_list_file = read.readfrom_json(preprocessed_file_path+ "_sent")
        pos_sentences_character = read.readfrom_json(preprocessed_file_path + "_pos")
        unico_sentences_characte = read.readfrom_json(preprocessed_file_path + "_unicodecategory")
        n_sent = len(sent_span_list_file)
        for index in range(n_sent):
            total +=1
            input_char.append(get_idx_from_sent("\n",sent_span_list_file[index][0], char2int, max_len,pad))
            input_pos.append(get_idx_from_sent("\n",pos_sentences_character[index], pos2int, max_len,pad))
            input_unic.append(get_idx_from_sent("Cc",unico_sentences_characte[index], unicode2int, max_len,pad))
        print("Finished processing file: ",raw_data_dir[data_id] )
    print total
    input_char = np.asarray(input_char, dtype="int")
    input_pos = np.asarray(input_pos, dtype="int")
    input_unic = np.asarray(input_unic, dtype="int")
    read.save_hdf5("/".join(output_folder.split('/')[:-1])+"/"+mode+"_input"+data_folder, ["char","pos","unic"], [input_char,input_pos,input_unic], ['int8','int8','int8'])

def output_encoding(raw_data_dir,output_folder,data_folder="",activation="softmax",type="interval",mode = "train"):   ###type in "[interval","operator","explicit_operator","implicit_operator"]
    if type not in ["interval","operator","explicit_operator","implicit_operator"]:
        return
    interval = read.textfile2list("data/config_data/label/non-operator.txt")
    operator = read.textfile2list("data/config_data/label/operator.txt")
    max_len = 350
    n_marks = 3
    max_len_text = 350+2*3
    n_output = 0
    final_labels = 0

    if activation == "sigmoid":
        final_labels = interval+operator
        n_output = len(final_labels)
    elif activation =="softmax":
        if "interval" in type:
            final_labels = interval
        elif "operator" in type:
            final_labels  = operator
        n_output = len(final_labels) +1

    one_hot = read.counterList2Dict(list(enumerate(final_labels, 1)))
    output_one_hot = {y:x for x,y in one_hot.iteritems()}

    sample_weights_output = []
    outputs = []
    total_with_timex =0
    for data_id in range(0, len(raw_data_dir)):
        preprocessed_file_path = os.path.join(preprocessed_path, file_dir[data_id], file_dir[data_id])
        sent_span_list_file = read.readfrom_json(preprocessed_file_path+ "_sent")
        tag_span_list_file = read.readfrom_json(preprocessed_file_path + "_tag")
        n_sent = len(tag_span_list_file)
        for index in range(n_sent):
            sent_info = sent_span_list_file[index]
            tag_info = tag_span_list_file[index]

            sentence_start = sent_info[1]
            label_encoding_sent = np.zeros((max_len_text, n_output))
            if activation == "softmax":
                label_encoding_sent[:, 0] = 1
            sample_weights_sent = np.zeros(max_len_text)
            #print tag_info
            for label in tag_info:
                posi, info = label
                position = int(posi) - sentence_start
                posi_end = int(info[0]) -sentence_start
                info_new = list(set(info[2:]))

                if activation == "sigmoid":

                    label_indices = [output_one_hot[token_tag] for token_tag in info_new if token_tag in output_one_hot]
                    k = np.sum(np.eye(n_output)[[sigmoid_index - 1 for sigmoid_index in label_indices]], axis=0)

                    label_encoding_sent[position + n_marks:posi_end + n_marks, :] = np.repeat([k], posi_end - position,axis=0)


                elif activation == "softmax":
                    if "explicit" or "interval" in type:
                        target_label = process.get_explict_label(info_new, interval, operator)
                    elif "implicit" in type:
                        target_label = process.get_implict_label(info_new, interval, operator)
                    label_indices = [output_one_hot[token_tag] for token_tag in target_label if token_tag in final_labels]
                    k = np.sum(np.eye(n_output)[[softmax_index for softmax_index in label_indices]], axis=0)
                    label_encoding_sent[position + n_marks:posi_end + n_marks, :] = np.repeat([k], posi_end - position,axis=0)
                t = len(label_indices)
                if t>=1:
                    sample_weights_sent[position + n_marks:posi_end + n_marks] = label_indices[randint(0, t - 1)]
            sample_weights_output.append(sample_weights_sent)
            outputs.append(label_encoding_sent)
            total_with_timex += 1
            print total_with_timex

    sample_weights = np.asarray(sample_weights_output)
    sample_weights = get_sample_weights_multiclass(n_output, sample_weights, 0.05)
    np.save("/".join(output_folder.split('/')[:-1])+"/"+mode+ "_sample_weights_"+type+"_"+activation+data_folder, sample_weights)
    read.save_hdf5("/".join(output_folder.split('/')[:-1]) +"/"+mode+"_output_"+type+"_"+activation+data_folder,[type+"_"+activation] , [outputs], ['int8'])

def main(file_dir,preprocessed_path,mode = ""):
    file_n = len(file_dir)
    folder_n = np.divide(file_n,20)
    folder = map(lambda x: int(x), np.linspace(0, file_n, folder_n + 1))
    if "split" in mode:
        for version in range(folder_n):
            start = folder[version]
            end = folder[version + 1]
            raw_data_dir_sub = file_dir[start:end]
            features_extraction(raw_data_dir_sub, preprocessed_path, data_folder=str(version),mode = mode)
            if "test" in mode:
                output_encoding(raw_data_dir_sub,preprocessed_path,data_folder = str(version),activation="softmax",type="interval",mode = mode)
                output_encoding(raw_data_dir_sub, preprocessed_path, data_folder=str(version), activation="softmax",type="explicit_operator",mode = mode)
                output_encoding(raw_data_dir_sub, preprocessed_path, data_folder=str(version),activation="softmax",type="implicit_operator" ,mode = mode)


    else:
        start = 0
        end = file_n
        raw_data_dir_sub = file_dir[start:end]
        features_extraction(raw_data_dir_sub, preprocessed_path,mode = mode)
        if mode !="" :
            output_encoding(raw_data_dir_sub, preprocessed_path,activation="softmax",type="interval",mode = mode)
            output_encoding(raw_data_dir_sub, preprocessed_path, activation="softmax",type="explicit_operator",mode = mode)
            output_encoding(raw_data_dir_sub, preprocessed_path,activation="softmax",type="implicit_operator" ,mode = mode)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Process features and output encoding for time identification task.')
    parser.add_argument('--raw',
                        help='raw data path',required=True)
    parser.add_argument('--file',
                        help='files required to be processed',default="")
    parser.add_argument('--xml',
                        help='specify anafora documents path if exist',default="")

    parser.add_argument('--out',
                        help='output path for all preprocessed files',required=True)

    parser.add_argument('--format',
                        help='output path for all preprocessed files',default=".TimeNorm.gold.completed.xml")
    parser.add_argument('--processed',
                        help='Whether requried to be processed',default="true")
    parser.add_argument('--mode',
                        help='Whether requried encode the output',default="")

    args = parser.parse_args()
    raw_data_path = args.raw
    file_list_name = args.file
    xml_path = args.xml
    preprocessed_path = args.out
    output_format = args.format
    documents_preprocessed = args.processed
    mode = args.mode
# raw_data_path = "data/THYMEColonFinal/Train"
# xml_path = "data/THYMEColonFinal/Train"
# preprocessed_path = "data/Processed_THYMEColonFinal/Train"
# output_format = ".TimeNorm.gold.completed.xml"
# documents_preprocessed = "false"
# test_file =[] #read.textfile2list("data/test_file.txt")[20:22]
# file_list_name = ""
# mode = "dev"


    if file_list_name == "":
        xml_path = "data/THYMEColonFinal/Train"
        test_file = [] #read.textfile2list("data/test_file.txt")[20:22]
        file_dir = get_xml_dir(xml_path, file_filters= test_file,has_root_folder=False,file_format = output_format )
    else:
        file_dir = read.textfile2list(file_list_name)

    #file_dir.remove("ID001_clinic_001")
    #file_dir = ["ID074_clinic_220"] # "ID010_clinic_030",
    if documents_preprocessed == "true":
        document_level_2_sentence_level(file_dir, raw_data_path, preprocessed_path,xml_path,file_format = output_format )
    main(file_dir[210:], preprocessed_path,mode = mode)






