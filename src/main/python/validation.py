import performance
from keras.callbacks import Callback
import numpy as np
from os import fsync

class Validation(Callback):
    def __init__(self, x, labels, filepath, tsv_logger):
        super().__init__()
        self.x = x
        self.gold_locs = list()
        for index in range(3):
            gold = performance.hot_vectors2class_index(labels[index])
            gold_loc = performance.found_location_with_constraint(gold)
            self.gold_locs.append(gold_loc)
        self.tsv_log = open(tsv_logger, 'w')
        self.best_model = filepath
        self.best_f1 = -1.0

    def on_epoch_end(self, epoch, logs=None):
        prediction = self.model.predict(self.x)
        classes = performance.prob2classes_multiclasses_multioutput(prediction)
        matches = list()
        tagged = list()
        precisions = list()
        recalls = list()
        f1s = list()
        golds = list()
        for index in range(3):
            class_loc = performance.found_location_with_constraint(classes[index])
            gold_loc = self.gold_locs[index]
            (n_pre, n_match, n_gold, precision, recall, f1) = performance.calculate_precision_multi_class(class_loc, gold_loc)
            matches.append(n_match)
            tagged.append(n_pre)
            precisions.append(precision)
            recalls.append(recall)
            f1s.append(f1)
            golds.append(n_gold)
        precision_overall, recall_overall, f1_overall = performance.socres(sum(matches), sum(tagged), sum(golds))

        self.tsv_log.write("Epoch: %s\tref: %s\tpred: %s\tcorr: %s\tP: %s\tR: %s\tF1: %s\n" % (epoch, sum(golds), sum(tagged), sum(matches), precision_overall, recall_overall, f1_overall))
        self.tsv_log.flush()
        fsync(self.tsv_log.fileno())

        if f1_overall > self.best_f1:
            self.best_f1 = f1_overall
            self.model.save(self.best_model)


    def on_train_end(self, logs=None):
        self.tsv_log.close()
