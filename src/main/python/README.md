# timeident

## Requirements
Python version: 3.6.2
Python Package: h5py, nltk 3.2.4, anaforatools, keras 2.2.0 , theano 1.0.2,regex 2.4.144

## Package issue
* StanfordPnOSTagger failed to tag the underscore, see https://github.com/nltk/nltk/issues/1632
* Please change the code #1 to #2 in nltk.standford.py:
```
#1 word_tags = tagged_word.strip().split(self._SEPARATOR)
#2 word_tags = tagged_word.strip().rsplit(self._SEPARATOR,1)
```



## Usages
* `preprocess.py` - Extract features from documents and generate the model input files.
* `output.py` - Generate the SCATE annofora annotation for the documents.
* `model_training.py` - Train a time entity identification models.




To processe the documents, please run:
```
$ python preprocess.py -raw "raw_documents" -xml "xml_path" -processed_output "the path for storing the processing files" -model_output "the_model_input_files"
```


To generate the SCATE anafora outputfiles:
```
$ python output.py python output.py -model "the_model_files" -raw "raw_documents" -processed_path "the path for storing the processing files" -input "model_inputs" -out "annonation outputs"
```

To train a time entity identification model:
```
$ python model_training.py es" -input "model_inputs" -out "model_outputs"
```

