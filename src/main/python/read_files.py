# encoding: utf-8
import json
import h5py
import numpy as np
import sys
import shutil
if sys.version_info[0]==2:
    import cPickle as pickle
else:
    import pickle
import os

def create_folder(filename):
    if "\\" in filename:
        a = '\\'.join(filename.split('\\')[:-1])
    else:
        a = '/'.join(filename.split('/')[:-1])
    if not os.path.exists(a):
        os.makedirs(a)



def savein_json(filename, array):
    create_folder(filename)
    with open(filename+'.txt', 'w') as outfile:
        json.dump(array, outfile)
    print("Save into files: ",filename)
    outfile.close()

def readfrom_json(filename):
    with open(filename+'.txt', 'r') as outfile:
        data = json.load(outfile)
    outfile.close()
    return data

def savein_pickle(file,array):
    create_folder(file)
    with open(file, 'wb') as handle:
        pickle.dump(array, handle)

def readfrom_pickle(file):
    with open(file, 'rb') as handle:
        if sys.version_info[0] == 2:
            data = pickle.load(handle)
        else:
            data = pickle.load(handle,encoding='latin1')
    return data

def readfrom_txt(path):
    data =open(path).read()
    return data

def textfile2list(path):
    data = readfrom_txt(path)
    txt_list =list()
    for line in data.splitlines():
        txt_list.append(line)
    return txt_list

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

def save_hdf5(filename,labels,data,dtypes):
    create_folder(filename)
    f = h5py.File(filename+ ".hdf5", "w")
    data_size = len(labels)
    for index in range(data_size):
        f.create_dataset(labels[index], data=data[index], dtype=dtypes[index])

def movefiles(dir_simples,old_address,new_address,abbr=""):
    for dir_simple in dir_simples:
        desti = dir_simple.replace(old_address,new_address)
        desti = desti.replace("TimeNorm.gold.completed.xml","TimeNorm.system.completed.xml")
        create_folder(desti)
        shutil.copy(dir_simple+abbr,desti)

def counterList2Dict (counter_list):
    dict_new = dict()
    for item in counter_list:
        dict_new[item[0]]=item[1]
    return dict_new

