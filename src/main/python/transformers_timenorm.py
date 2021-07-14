import anafora
import argparse
import collections
import numpy as np
import os
import spacy
import transformers
import tensorflow as tf

time_types = [
    None,
    'AMPM-Of-Day',
    'After',
    'Before',
    'Between',
    'Calendar-Interval',
    'Day-Of-Month',
    'Day-Of-Week',
    'Frequency',
    'Hour-Of-Day',
    'Intersection',
    'Last',
    'Minute-Of-Hour',
    'Modifier',
    'Month-Of-Year',
    'Next',
    'NthFromStart',
    'Number',
    'Part-Of-Day',
    'Part-Of-Week',
    'Period',
    'Season-Of-Year',
    'Second-Of-Minute',
    'Sum',
    'This',
    'Time-Zone',
    'Two-Digit-Year',
    'Union',
    'Year',
]
operator_types = {
    'After',
    'Before',
    'Between',
    'Frequency',
    'Intersection',
    'Last',
    'Next',
    'NthFromStart',
    'Sum',
    'This',
    'Union',
}
time_type_to_index = {l: i for i, l in enumerate(time_types)}

nlp = spacy.load("en_core_web_lg")
tokenizer = transformers.AutoTokenizer.from_pretrained("roberta-base")


def iter_anafora(anafora_root_dir):
    for sub_dir, text_file_name, [xml_file_name] in anafora.walk(
            anafora_root_dir, "TimeNorm[.]gold[.]completed[.]xml$"):
        text_path = os.path.join(anafora_root_dir, sub_dir, text_file_name)
        with open(text_path) as text_file:
            text = text_file.read()
        yield sub_dir, text_file_name, xml_file_name, text.rstrip()


def collect_labeled_data(anafora_root_dir):
    sentences = []
    sentence_texts = {}
    sentence_char_labels = {}
    for sub_dir, _, xml_file_name, text in iter_anafora(anafora_root_dir):
        xml_path = os.path.join(anafora_root_dir, sub_dir, xml_file_name)
        data = anafora.AnaforaData.from_file(xml_path)
        char_labels = collections.defaultdict(set)
        for annotation in data.annotations:
            (start, end), = annotation.spans
            if annotation.type not in {"Event", "NotNormalizable"}:
                for i in range(start, end):
                    char_labels[i].add(annotation.type)

        doc = nlp(text)
        for sentence in doc.sents:
            sentences.append(sentence)
            sentence_texts[sentence] = text
            sentence_char_labels[sentence] = char_labels

    inputs = tokenize([sentence.text for sentence in sentences])

    labels = np.zeros(inputs["input_ids"].shape)
    for i, sentence, token_tuples in iter_tokens(inputs, sentences):
        text = sentence_texts[sentence]
        char_labels = sentence_char_labels[sentence]

        for j, token_id, start, end in token_tuples:
            # sanity check for mismatch between text and word-piece
            word_piece = tokenizer.decode(token_id)
            if token_id not in tokenizer.all_special_ids and text[start:end] != word_piece.lstrip(' '):
                raise ValueError(f"{text[start:end]!r} != {word_piece!r}")

            # find labels for the given offsets
            token_labels = {x for c in range(start, end)
                            for x in char_labels[c] or {None}}
            non_op_labels = {x for x in token_labels if x not in operator_types}
            if not token_labels:
                token_label = None
            elif len(token_labels) == 1:
                token_label = token_labels.pop()
            elif len(non_op_labels) == 1:
                token_label = non_op_labels.pop()
            else:
                context = f"{text[start-5:start]}[{text[start:end]}]"\
                          f"{text[end:end+5]}"
                print(f"Skipping: {context!r} {token_labels}")
                token_label = None
            labels[i][j] = time_type_to_index[token_label]
            if token_id not in tokenizer.all_special_ids and token_label:
                print(f'{start}:{end} {text[start:end]!r} {token_label} {labels[i][j]}')

    return tf.data.Dataset.from_tensor_slices((
        dict(inputs),
        tf.constant(labels),
    ))


def tokenize(texts):
    return tokenizer(texts,
                     padding="longest",
                     return_tensors="tf",
                     return_offsets_mapping=True)


def iter_tokens(inputs, sentences):
    for sent_index, sentence in enumerate(sentences):
        token_tuples = []
        for token_index in range(inputs["input_ids"].shape[1]):
            offsets = inputs["offset_mapping"][sent_index][token_index].numpy()
            start, end = [sentence.start_char + o for o in offsets]
            token_id = inputs["input_ids"][sent_index][token_index]
            token_tuples.append((token_index, token_id, start, end))
        yield sent_index, sentence, token_tuples


def train(model_name, train_dir):
    train_data = collect_labeled_data(train_dir)
    print(f'training on {train_data}')
    model = transformers.TFAutoModelForTokenClassification.from_pretrained(
        "roberta-base", num_labels=len(time_types))
    model.summary()
    optimizer = tf.keras.optimizers.Adam(learning_rate=1e-5)
    model.compile(optimizer=optimizer, loss=model.compute_loss,
                  metrics=[tf.keras.metrics.SparseCategoricalAccuracy()])
    model.fit(train_data.batch(4), epochs=100)
    model.save_pretrained(model_name)


def _can_merge(text, entity, label, start):
    if entity is None:
        return False
    if label != entity.type:
        return False
    (_, last_end), = entity.spans
    intervening_text = text[last_end:start]
    return not intervening_text or intervening_text.isspace()


def test(model_name, test_dir, output_dir):
    model = transformers.TFAutoModelForTokenClassification.from_pretrained(
        model_name, num_labels=len(time_types))

    for sub_dir, text_file_name, xml_file_name, text in iter_anafora(test_dir):
        sentences = [sentence for sentence in nlp(text).sents]
        inputs = tokenize([sentence.text for sentence in sentences])
        print(f'predicting on {tf.data.Dataset.from_tensor_slices(inputs)}')
        logits = model(inputs)["logits"]
        labels = tf.math.argmax(logits, axis=-1)
        data = anafora.AnaforaData()
        n = 1
        entity = None
        for i, sentence, token_tuples in iter_tokens(inputs, sentences):
            for j, token_id, start, end in token_tuples:
                if token_id not in tokenizer.all_special_ids:
                    label = time_types[labels[i][j]]
                    if label is not None:
                        print(f'{start}:{end} {label} {text[start:end]!r}')
                        if _can_merge(text, entity, label, start):
                            (start, _), = entity.spans
                        else:
                            entity = anafora.AnaforaEntity()
                            entity.id = f"{n}@e@{text_file_name}@system"
                            entity.type = label
                            data.annotations.append(entity)
                            n += 1
                        entity.spans = (start, end),

        output_sub_dir = os.path.join(output_dir, sub_dir)
        if not os.path.exists(output_sub_dir):
            os.makedirs(output_sub_dir, exist_ok=True)
        xml_file_name = xml_file_name.replace("gold", "system")
        data.to_file(os.path.join(output_sub_dir, xml_file_name))


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers()
    train_parser = subparsers.add_parser("train")
    train_parser.add_argument("model_name")
    train_parser.add_argument("train_dir")
    train_parser.set_defaults(func=train)
    test_parser = subparsers.add_parser("test")
    test_parser.add_argument("model_name")
    test_parser.add_argument("test_dir")
    test_parser.add_argument("output_dir")
    test_parser.set_defaults(func=test)
    args = vars(parser.parse_args())
    args.pop("func")(**args)
