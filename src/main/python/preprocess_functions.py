# encoding: utf-8
import re
from nltk.tokenize import sent_tokenize
from nltk.tag.stanford import StanfordPOSTagger
import nltk
import anafora
from collections import OrderedDict
from collections import deque


def addannotation_to_dict(posi_info_dict,annotation,raw_text):
    if posi_info_dict.has_key(annotation.spans[0][0]):
            posi_info_dict[annotation.spans[0][0]].append(annotation.type)
    else:
        anna_info = []
        #print annotation.spans[0][0], annotation.spans[0][1]
        terms = raw_text[annotation.spans[0][0]:annotation.spans[0][1]]
        anna_info.append(annotation.spans[0][1])
        anna_info.append(terms)
        anna_info.append(annotation.type)
        posi_info_dict[annotation.spans[0][0]] = anna_info
    return posi_info_dict

def extract_xmltag_timeml(xml_file_dir,raw_text):
    #annotation = ["TIMEX3"]
    time_annotation = ["DATE","TIME","DURATION","SET"]
    data = anafora.AnaforaData.from_file(xml_file_dir)
    posi_info_dict = dict()
    for annotation in data.annotations:
        property = annotation.properties._tag_to_property_xml
        if property.has_key("type"):
            type = property["type"].text
            if type in time_annotation:
                addannotation_to_dict(posi_info_dict,annotation,raw_text)
    posi_info_dict = OrderedDict(sorted(posi_info_dict.items()))
    return posi_info_dict

def extract_xmltag_anafora(xml_file_dir,raw_text):
    delete_annotation = ["Event","Modifier","PreAnnotation","NotNormalizable"]
    data = anafora.AnaforaData.from_file(xml_file_dir)
    posi_info_dict = dict()
    for annotation in data.annotations:
        if annotation.type not in delete_annotation:
            posi_info_dict = addannotation_to_dict(posi_info_dict,annotation,raw_text)
    posi_info_dict = OrderedDict(sorted(posi_info_dict.items()))
    return posi_info_dict

def extract_xmltag_anafora_pred(xml_file_dir,raw_text):
    data = anafora.AnaforaData.from_file(xml_file_dir)
    posi_info_dict = dict()
    for annotation in data.annotations:

            if posi_info_dict.has_key(annotation.spans[0][0]):
                posi_info_dict[annotation.spans[0][0]].append([annotation.spans[0][1],annotation.type])
            else:
                anna_info = []
                print annotation.spans[0][0], annotation.spans[0][1]
                terms = raw_text[annotation.spans[0][0]:annotation.spans[0][1]]
                anna_info.append(terms)
                anna_info.append([annotation.spans[0][1],annotation.type])
                posi_info_dict[annotation.spans[0][0]] = anna_info
    posi_info_dict = OrderedDict(sorted(posi_info_dict.items()))
    return posi_info_dict

def get_explict_label(result,explicit_labels1,explicit_labels2):

    if len(result) >1:
        intersection1 = [x for x in result if x in explicit_labels1]
        intersection2 = [x for x in result if x in explicit_labels2]

        if len(intersection1) ==1:
            result = intersection1
        # elif len(intersection1) > 1:
        #     implicit_interval = ["Frequency"]
        #     result = [x for x in intersection1 if x not in implicit_interval]

        elif len(intersection2) >1:    ## to do: [u'3', u'Number', u'Period', u'Sum']   He won the Gusher Marathon, finishing in 3:07:35.
            implicit_operator = ["Intersection"]
            result = [x for x in intersection2 if x not in implicit_operator]
    return [result[0]]

def get_implict_label(result,explicit_labels1,explicit_labels2):
    #print result
    if len(result) >1:
        intersection1 = [x for x in result if x in explicit_labels1]
        intersection2 = [x for x in result if x in explicit_labels2]

        if len(intersection2) ==1:
            result = intersection2
        elif len(intersection2) >1 and len(intersection1) >0:
            implicit_operator = ["Intersection"]
            result = [x for x in intersection2 if x not in implicit_operator]
        elif len(intersection2) >1 and len(intersection1) ==0:   ## to do sum treated as implicit and explicit: [u'3', u'Number', u'Period', u'Sum']   He won the Gusher Marathon, finishing in 3:07:35.
            implicit_operator = ["Intersection"]   ###special case
            result = [x for x in intersection2 if x in implicit_operator]
            if len(result) ==0:
                result = intersection2
        return [result[0]]
    else:
        return "null"


def text_normalize(rawtext):
    # rawtext = rawtext.replace("•", "\n")
    # rawtext = rawtext.replace("’", "\'")
    # rawtext = rawtext.replace("”", "\"")
    # rawtext = rawtext.replace("“", "\"")
    # rawtext = rawtext.replace("‘", "\'")
    # rawtext = rawtext.replace("—", "-")
    # rawtext = rawtext.replace("¢", "c")
    # rawtext = rawtext.replace("°", "\'")
    # rawtext = rawtext.replace("%", "%")
    # rawtext = rawtext.replace("$", "$")
    return rawtext

def spans(sents,txt):
    sentence_chunkings = list()
    offset = 0
    for sent in sents:
        offset = txt.find(sent, offset)
        item = (sent, offset, offset + len(sent))
        offset += len(sent)
        sentence_chunkings.append(item)
    return sentence_chunkings

def add_start_end(sent_tokenize_span_list,start):
    sent_tokenize_span_list_new = list()
    max_len = list()
    for (sent,sent_start,sent_end) in sent_tokenize_span_list:
        sent_tokenize_span_list_new.append((sent,sent_start+start,sent_end+start))
        max_len.append(sent_end-sent_start)
    return sent_tokenize_span_list_new,max_len

def split_sentence_based_on_rules(sent):


    if re.search(r' \.+ ', sent):
        sentences = re.split(r' \.+ ', sent)
    elif re.search(r'@ ---- @', sent):
        sentences = re.split(r'@ ---- @', sent)
    elif re.search(r'\.\w+\:', sent):
        sent = re.sub(r'\.(\w+)\:', r'. \1:', sent)
        sentences = sent_tokenize(sent)
    elif re.search(r'\, as well as', sent):
        sent = sent.replace(', as well as', '. As well as')
        sentences = sent_tokenize(sent)
    elif re.search(r'[a-z\.]+[A-Z][a-z]+:', sent):
        k = re.findall(r' [a-z\.]+([A-Z][a-z]+:)', sent)
        p = chr(ord(max(sent)) + 1)
        sentences = sent.replace(k[0], p + k[0]).split(p)
    elif re.search(r'\; ', sent):
        sent = re.sub(r'\; ', r'. ', sent)
        sentences = sent_tokenize(sent)
    elif re.search(r', and, ', sent):
        sent = sent.replace(', and, ', '. And, ')
        sentences = sent_tokenize(sent)
    elif re.search(r'president\: Wechsler', sent):
        sent = sent.replace(': ', '. ')
        sentences = sent_tokenize(sent)
    elif re.search(r'\, ', sent):
        sentences = re.split(r'\, ', sent)
    else:
        sentences = [sent[:349],sent[350:]]
        print("Using greedy sentence tokenization")

    text_len = [len(sentence) for sentence in sentences]
    return sentences


def rule_based_tokenizer(sent_ori,sent_span): # sent,sent_span

    #sent = "@ % This @ Oct 25 Oct 24 Year @ U.S. ................... 315.2 316.4 +23.1 @ Britain ................ 646.4 643.1 +18.4 @ Canada ................. 426.9 426.4 +16.3 @ Japan .................. 1547.1 1550.9 + 8.9 @ France ................. 518.6 521.2 +17.1 @ Germany ................ 236.7 241.0 +13.8 @ Hong Kong .............. 2049.2 2068.9 + 1.0 @ Switzerland ............ 212.6 216.5 +23.0 @ Australia .............. 326.0 329.4 +12.3 @ World index ............ 532.4 533.4 + 7.7 @ Weekly Percentage Leaders"
    #sent = "U.S. Attorney Denise E. O'Donnell declined to discuss what federal charges were being pursued, but she said that in a case like this, potential charges would be abortion-related violence, the use of a firearm in an act of violence, crossing state lines to commit a crime, and, if the suspect's act was tied to an organization, violation of the so-called RICO statutes, which prohibit an organized criminal enterprise."
    #sent = "WASHINGTON _ Following are statements made Friday and Thursday by Lawrence Wechsler, a lawyer for the White House secretary, Betty Currie; the White House; White House spokesman Mike McCurry, and President Clinton in response to an article in The New York Times on Friday about her statements regarding a meeting with the president: Wechsler on Thursday ``Without commenting on the allegations raised in this article, to the extent that there is any implication or suggestion that Mrs. Currie was aware of any legal or ethical impropriety by anyone, that implication or suggestion is entirely inaccurate.''"
    #sent = "Thursday's Markets: @ Earnings @ Data Cause @ Stock Fall @ --- @ Industrials Sink 39.55; @ Bonds Slip, but Dollar @ Soars Against Pound @ ---- @ By Douglas R. Sease @ Staff Reporter of The Wall Street Journal 10/27/89 WALL STREET JOURNAL (J) MONETARY NEWS, FOREIGN EXCHANGE, TRADE (MON) STOCK INDEXES (NDX) STOCK MARKET, OFFERINGS (STK) FINANCIAL, ACCOUNTING, LEASING (FIN) BOND MARKET NEWS (BON) FOREIGN-EXCHANGE MARKETS (FRX) TREASURY DEPARTMENT (TRE)"
    #start = 20
    #end = 20 + len(sent)
    text_len = []
    start, end  = sent_span
    sentences = deque([sent_ori])
    sent_output = []
    while sentences.__len__() > 0:
        sent = sentences.popleft()
        if len(sent) >= 350:
            sentences_temp = split_sentence_based_on_rules(sent)
            for sentence_temp in sentences_temp:
                sentences.append(sentence_temp)
        else:
            sent_output.append(sent)


    sent_tokenize_span_list = spans(sent_output, sent_ori)
    sent_tokenize_span_list , max_len = add_start_end(sent_tokenize_span_list,start)
    #print sent_tokenize_span_list,max_len
    return sent_tokenize_span_list , max_len

def get_pos_sentence(sentences_spans,pos_vocab):
    """
    Get POS tags for each sentence. (needed to build end2end system)
    :param start:
    :param end:
    :return:
    """
    #raw_dir_simple = read.read_from_json('test/test_dir_simple')   #### in folder data/
    #raw_dir_simple = read.read_from_json('clinical_data/train_samples1_simples')
    #raw_dir_simple = read.read_from_json('agriculture_data/raw_dir_simple')

    #raw_dir_simple = ["NYT19980206.0466"]
    english_postagger = StanfordPOSTagger(
        'C:/Users/dongfangxu9/PycharmProjects/pos_tagger/models/english-left3words-distsim.tagger',    #### in folder data/
        'C:/Users/dongfangxu9/PycharmProjects/pos_tagger/stanford-postagger.jar') #### in folder data/
    english_postagger.java_options = '-mx8000m'
    pos_sentences = list()

    for sent_span in sentences_spans:
        print sent_span[0]
        text = nltk.word_tokenize(sent_span[0])
        text_pos = english_postagger.tag(text)   #####StanfordPnOSTagger failed to tag the underscore, see ttps://github.com/nltk/nltk/issues/1632  if use nltk 3.2.2, please change the code "word_tags = tagged_word.strip().split(self._SEPARATOR)" in function "parse_outputcode" of nltk.standford.py to "word_tags = tagged_word.strip().rsplit(self._SEPARATOR,1)" to handle undersocre issues

        index = 0
        for token in text_pos:
            # if (text[index] != token[0]) and (token[0] == '``' or token[0] == "''"):  ######### deal with the double quotes, in nltk.tokenize treebank.py change the tokenizer for double quotes. Reasons: (double quotes (") are changed to doubled single forward- and backward- quotes (`` and ''))
            #     text_pos[index] = ["\"", "\'\'"]
            if text[index] == token[0] and token[0] == "``"  and text[index] not in sent_span[0]:
                text_pos[index] = ["\"", "``"]
            if text[index] ==token[0] and token[0] == "''"  and text[index] not in sent_span[0]:
                text_pos[index] = ["\"", "\'\'"]
            if text[index] == token[0] and token[0] in ['{','(','['] :
                text_pos[index] = [token[0],"("]
            if text[index] == token[0] and token[0] in ['}',')',']']:
                text_pos[index] = [token[0],")"]
            pos_vocab[token[1]]+=1
            index+=1
        pos_sentences.append(text_pos)
    return pos_sentences,pos_vocab

def word_pos_2_character_pos(sentences_spans,pos_sentences):
    pos_sentences_character = list()
    for sent_index in range(len(sentences_spans)):
        post_sentence_character = list()
        token_index = 0
        term = ""
        for char in sentences_spans[sent_index][0]:
            if char == ' ' or char == '\t':
                term = ""
                post_sentence_character.append("null")
            else:
                term += char
                if term in pos_sentences[sent_index][token_index][0] and len(term) < len(pos_sentences[sent_index][token_index][0]):
                    if bool(re.compile(r'[\/\:\-]').match(char)):
                        if len(term) == 1:
                            post_sentence_character.append(pos_sentences[sent_index][token_index][1])
                        else:
                            post_sentence_character.append('Sep')
                    else:
                        post_sentence_character.append(pos_sentences[sent_index][token_index][1])
                elif term in pos_sentences[sent_index][token_index][0] and len(term) == len(pos_sentences[sent_index][token_index][0]):
                    # if pos[index][token_index][1] =="CD" and bool(re.compile(r'[/\:\-]').match(char)):
                    #     postag.append('Sep')
                    # else:
                    post_sentence_character.append(pos_sentences[sent_index][token_index][1])
                    token_index += 1
                    term = ""
                    if token_index == len(pos_sentences[sent_index]):
                        print post_sentence_character
                        pos_sentences_character.append(post_sentence_character)
    if len(pos_sentences_character) != len(sentences_spans):
        print "Transformation from word_pos to character_pos failed."
    return pos_sentences_character

def get_unicode(sentences_spans,unicode_vocab):
    import unicodedata
    unicate_sentences = list()
    for sent in sentences_spans:
        unicate_sentence = []
        for char in sent[0]:
            char_unic  = unicodedata.category(char.decode("utf-8"))
            unicate_sentence.append(char_unic)
            unicode_vocab[char_unic] +=1
        unicate_sentences.append(unicate_sentence)
    return unicate_sentences, unicode_vocab


