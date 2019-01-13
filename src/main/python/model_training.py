import numpy as np
import h5py
np.random.seed(20181117)
from keras.layers.wrappers import Bidirectional
from keras.layers import Dense
import argparse
import configparser
from keras.layers import Embedding, LSTM, Input, Lambda, Concatenate
from keras.callbacks import CSVLogger
from keras.regularizers import l2
from keras.models import Model,load_model
import keras.backend as K
from keras.callbacks import ModelCheckpoint
import os



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


def trainging(storage, flair_path, sampleweights,char_x,trainy_interval,trainy_operator_ex,trainy_operator_im,
            char_x_cv, cv_y_interval, cv_y_operator_ex, cv_y_operator_im,batchsize,epoch_size,
              gru_size1 =256,gru_size2 = 150):

    seq_length = char_x.shape[1]
    type_size_interval = trainy_interval.shape[-1]
    type_size_operator_ex = trainy_operator_ex.shape[-1]
    type_size_operator_im = trainy_operator_im.shape[-1]

    if not os.path.exists(storage):
        os.makedirs(storage)
        os.makedirs(storage+"/model")

    char_input = Input(shape=(seq_length,), dtype='int8', name='character')
    forward_embedding_layer = Embedding(input_dim=227, output_dim=100)(char_input)
    forward_lstm_layer = LSTM(2048, return_sequences=True, recurrent_activation='sigmoid')(forward_embedding_layer)

    backward_embedding_layer = Embedding(input_dim=227, output_dim=100)(char_input)
    backward_lstm_layer = LSTM(2048, return_sequences=True, recurrent_activation='sigmoid', go_backwards=True)(backward_embedding_layer)
    reversed_backward_lstm_layer = Lambda(lambda tensor: K.reverse(tensor, axes=1),output_shape=(356,2048))(backward_lstm_layer)
    merged_lstm_layers = Concatenate(axis=2)([forward_lstm_layer, reversed_backward_lstm_layer])

    Gru_out_1 = Bidirectional(LSTM(gru_size1, return_sequences=True))
    Gru_out_2 = LSTM(gru_size2, return_sequences=True)

    Gru_out_3 = Bidirectional(LSTM(gru_size1, return_sequences=True))
    Gru_out_4 = LSTM(gru_size2, return_sequences=True)

    Gru_out_5 = Bidirectional(LSTM(gru_size1, return_sequences=True))
    Gru_out_6 = LSTM(gru_size2, return_sequences=True)

    Interval_output = Dense(type_size_interval, activation='softmax', kernel_regularizer=l2(.01), name='dense_1')

    Explicit_operator = Dense(type_size_operator_ex, activation='softmax', kernel_regularizer=l2(.01),
                              name='dense_2')

    Implicit_operator = Dense(type_size_operator_im, activation='softmax', kernel_regularizer=l2(.01),
                              name='dense_3')

    gru_out1 = Gru_out_1(merged_lstm_layers)
    gru_out2 = Gru_out_2(gru_out1)
    interval_output = Interval_output(gru_out2)

    gru_out3 = Gru_out_3(merged_lstm_layers)
    gru_out4 = Gru_out_4(gru_out3)
    explicit_operator = Explicit_operator(gru_out4)

    gru_out5 = Gru_out_5(merged_lstm_layers)
    gru_out6 = Gru_out_6(gru_out5)
    implicit_operator = Implicit_operator(gru_out6)

    model = Model(inputs=char_input,
                  outputs=[interval_output, explicit_operator, implicit_operator])

    model.layers[2].trainable = False
    model.layers[1].trainable = False
    model.layers[4].trainable = False
    model.layers[3].trainable = False

    model.compile(optimizer='rmsprop',
                  loss={'dense_1': 'categorical_crossentropy',
                        'dense_2': 'categorical_crossentropy',
                        'dense_3': 'categorical_crossentropy'},
                  loss_weights={'dense_1': 1.0, 'dense_2': 0.75, 'dense_3': 0.5},
                  metrics=['categorical_accuracy'],
                  sample_weight_mode="temporal")

    model_flair = load_model(flair_path)
    model.layers[2].set_weights(model_flair.layers[2].get_weights())
    model.layers[1].set_weights(model_flair.layers[1].get_weights())
    model.layers[4].set_weights(model_flair.layers[4].get_weights())
    model.layers[3].set_weights(model_flair.layers[3].get_weights())

    print(model.summary())
    filepath = storage + "model/weights-improvement-{epoch:02d}.hdf5"
    checkpoint = ModelCheckpoint(filepath, verbose=0, save_best_only=False)

    csv_logger = CSVLogger(storage+ '/training_log.csv')
    callbacks_list = [checkpoint,csv_logger]

    hist = model.fit(x ={'character': char_x},
                     y={'dense_1': trainy_interval, 'dense_2': trainy_operator_ex,'dense_3': trainy_operator_im}, epochs=epoch_size,
                     batch_size=batchsize, callbacks=callbacks_list, validation_data =({'character': char_x_cv},{'dense_1': cv_y_interval,
                    'dense_2': cv_y_operator_ex, 'dense_3': cv_y_operator_im}),sample_weight=sampleweights)
    model.save(storage + '/model/model_result.hdf5')
    np.save(storage + '/model/epoch_history.npy', hist.history)

if __name__ == "__main__":

    config = configparser.ConfigParser()
    config.read('ident.conf')
    ##########################The flair model in keras, please check the UA box, and download the model "flair_keras.h5"
    flair_path = config['Flair_Model']['Flair']


    parser = argparse.ArgumentParser(description='Train a time entity identification model')

    parser.add_argument('-input',
                        help='the direcotory of inputs', default="")

    parser.add_argument('-output',
                        help='the direcotory of outputs', default="")

    args = parser.parse_args()
    input_path = args.input
    output_path = args.output

    char_x = load_hdf5(input_path + "/input", ["char"])[0]
    char_x_cv = load_hdf5(input_path + "/cvinput", ["char"])[0]

    trainy_interval = load_hdf5(input_path + "/cvoutput_interval_softmax", ["interval_softmax"])[0]
    trainy_operator_ex = load_hdf5(input_path + "/cvoutput_explicit_operator_softmax", ["explicit_operator"])[0]
    trainy_operator_im = load_hdf5(input_path + "/cvoutput_implicit_operator_softmax", ["implicit_operator"])[0]

    cv_y_interval = load_hdf5(input_path + "/cvoutput_interval_softmax", ["interval_softmax"])[0]
    cv_y_operator_ex = load_hdf5(input_path + "/cvoutput_explicit_operator_softmax", ["explicit_operator"])[0]
    cv_y_operator_im = load_hdf5(input_path + "/cvoutput_implicit_operator_softmax", ["implicit_operator"])[0]

    sampleweights_interval = np.load(input_path+"/sample_weights_interval_softmax.npy")
    sampleweights_explicit_operator = np.load(input_path + "/sample_weights_explicit_operator_softmax.npy")
    sampleweights_implicit_operator = np.load(input_path + "/sample_weights_implicit_operator_softmax.npy")
    sampleweights = [sampleweights_interval,sampleweights_explicit_operator,sampleweights_implicit_operator]


    epoch_size = 400
    batchsize = 128



    trainging(output_path,flair_path,sampleweights,char_x,trainy_interval,trainy_operator_ex,trainy_operator_im,
              char_x_cv, cv_y_interval, cv_y_operator_ex, cv_y_operator_im,batchsize,epoch_size,
              gru_size1 =256,gru_size2 = 150)
