# CHARACTER BASED HISTOGRAM

import anafora
import os 
import matplotlib.pyplot as plt
import numpy as np 
import operator


def sort_dictionary(dict_):
    return dict( sorted(dict_.items(), key=operator.itemgetter(1),reverse=True))


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


original_dir = "/Users/bulut/timenorm-garage/tempeval-2013-replicate/Train"
char_histogram_dir = "/Users/bulut/timenorm-garage/relation-extraction/character-based-histograms/"
combined = {} # saves text and annotations of a single text file in each key

for text_path, text, xml_path, data in iter_data(original_dir, "gold"):
        text_file_name = text_path.split('/')[-1]
        combined[text_file_name] = {"annotation": data}

char_based_histograms = {}

for k,v in combined.items():
    print("filename: ", k)
    start_char_per_id = {}
    for entity in v["annotation"].annotations:
        entity_id = entity.xml.find('id').text.split('@')[0]
        entity_spans = entity.xml.find('span').text
        start, end = [int(index) for index in entity_spans.split(',')]
        start_char_per_id[entity_id] = start

    for entity in v["annotation"].annotations:
        entity_spans = entity.xml.find('span').text
        start, _ = [int(index) for index in entity_spans.split(',')]
        for prop in entity.properties.xml.iter():
            relation = prop.tag
            related_entity = prop.text
            if type(related_entity) == str and '@' in related_entity:
                if relation not in char_based_histograms.keys():
                    char_based_histograms[relation] = {}
                related_entity_id = related_entity.split('@')[0]
                related_entity_start = start_char_per_id[related_entity_id]
                distance = start - related_entity_start
                if distance in char_based_histograms[relation].keys():
                    char_based_histograms[relation][distance] += 1
                else:
                    char_based_histograms[relation][distance] = 1


for relation in char_based_histograms.keys():
    print(char_based_histograms[relation])
    char_based_histograms[relation] = sort_dictionary(char_based_histograms[relation])
    print(char_based_histograms[relation])
    numeric_distances = list(char_based_histograms[relation].keys())
    distances = [str(d) for d in numeric_distances]
    values = list(char_based_histograms[relation].values())
      
    fig = plt.figure(figsize = (15, 10))
     
    # creating the bar plots 
    plt.bar(distances, values, width = 0.15, color ='maroon', align='edge')
     
    #ax = plt.gca()
    #ax.set_xticklabels(distances, rotation=40, ha="right")

    #plt.xticks(rotation=45)
    plt.xlabel("character based distances")
    plt.ylabel("# of observations")
    plt.yticks(np.arange(min(values), max(values)+1, 10.0))

    plt.title(f"histogram of character based distances for {relation}")
    plt.tight_layout()
    plt.savefig(os.path.join(char_histogram_dir, f"{relation}.png"), dpi=400, pad_inches=0)
















