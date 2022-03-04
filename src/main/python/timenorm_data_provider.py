import os
from typing import Tuple, List

import anafora
import spacy
from transformers import PreTrainedTokenizerFast, AutoTokenizer


class TimeDataProvider:
    def __init__(self,
                 corpus_dir: str,

                 ) -> None:

        if not os.path.exists(corpus_dir):
            raise Exception(f"The {corpus_dir} does not exit.")
        self.corpus_dir = corpus_dir

    @staticmethod
    def iter_data(root_dir: str, xml_type: str):
        for dir_path, dir_names, file_names in os.walk(root_dir):
            if not dir_names:

                # read the text from the text file
                [text_file_name] = [f for f in file_names if not f.endswith(".xml")]
                text_path = os.path.join(dir_path, text_file_name)
                with open(text_path) as text_file:
                    text = text_file.read()

                # calculate the XML file name from the text file name
                xml_path = f"{text_path}.TimeNorm.{xml_type}.completed.xml"

                # read the gold annotations or create an empty annotations object
                if xml_type == 'gold':
                    data = anafora.AnaforaData.from_file(xml_path)
                elif xml_type == 'system':
                    data = anafora.AnaforaData()
                else:
                    raise ValueError(f"unsupported xml_type: {xml_type}")

                # generate a tuple for this document
                yield text_path, text, xml_path, data

    def read_data_to_pinter_seqs_format(self,
                                        fast_tokenizer: PreTrainedTokenizerFast,
                                        max_length: int) -> Tuple[List[str], List[str]]:
        """Read the text and Anafora annotations to source and target sequence
        The source sequence is plain text.
        The format of the target sequence is: ... <m> <ptr{entity_mention_token_index}> </m> <t> {entity_type} </t> ...
        """
        all_src_seqs = []
        all_tgt_seqs = []

        for _, text, _, data in self.iter_data(self.corpus_dir, "gold"):
            spans_types = {
                span: annotation.type
                for annotation in data.annotations
                for span in annotation.spans
            }

            # Sort the annotations by start span
            def get_start_offset(span_type):
                (_start, _end), types = span_type
                return _start

            spans_types = dict(sorted(spans_types.items(), key=get_start_offset))

            # Split the document into sentences, performed by DependencyParser in spacy
            nlp = spacy.load("en_core_web_sm")
            doc = nlp(text)

            for sent in doc.sents:
                sent_start_char, sent_end_char = sent.start_char, sent.end_char

                # Tokenize the sentence and get char level offsets
                tokenized_features = fast_tokenizer(sent.text,
                                                    return_offsets_mapping=True,
                                                    add_special_tokens=False)
                tokens_char_offsets = tokenized_features['offset_mapping']

                # Create the mapping
                char_offset_to_token_index = [-1 for _ in sent.text]

                # For each token's start and end offset
                for i, (start, end) in enumerate(tokens_char_offsets):

                    # fill the token index from start to end in the list
                    for j in range(start, end):
                        char_offset_to_token_index[j] = i

                # target seq
                tgt_seq = []

                # Find the annotations spans corresponds to this sentence
                for span, entity_type in spans_types.items():
                    if span[0] >= sent_start_char and span[1] <= sent_end_char:

                        # Get the entity's span for the entity in current sentence
                        entity_sent_char_start = span[0] - sent_start_char
                        entity_sent_char_end = span[1] - sent_start_char

                        # Left close right open for indexing
                        entity_sent_char_end -= 1

                        # start and end token index
                        start_token_index = char_offset_to_token_index[entity_sent_char_start]
                        end_token_index = char_offset_to_token_index[entity_sent_char_end]

                        # Make sure the entity's spans are less than our max_length -1
                        if start_token_index >= max_length - 1 or end_token_index >= max_length - 1:
                            continue

                        # Assert start and end position index is not -1
                        assert start_token_index != -1
                        assert end_token_index != -1

                        # create the pointer tokens
                        ptr_tokens = " ".join([f"<ptr{i}>" for i in range(start_token_index, end_token_index + 1)])
                        ptr_tokens = f" <m> {ptr_tokens} </m> <t> {entity_type} </t>"
                        tgt_seq.append(ptr_tokens)
                all_tgt_seqs.append(''.join(tgt_seq))
                all_src_seqs.append(sent.text)

        return all_src_seqs, all_tgt_seqs