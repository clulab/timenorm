# error analysis

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

#predicted.withlinks.xml

def iter_data_predictions(root_dir: str):
    for dir_path, dir_names, file_names in os.walk(root_dir):
        if not dir_names:

            # read the text from the text file
            [xml_file_name] = [f for f in file_names if f.endswith(".xml")]
            xml_path = os.path.join(dir_path, xml_file_name)

            data = anafora.AnaforaData.from_file(xml_path)

            # generate a tuple for this document
            yield xml_path, data

prediction_dir = "/Users/bulut/timenorm-garage/relation-extraction/egoitz-output"
original_dir = "/Users/bulut/timenorm-garage/tempeval-2013-replicate/Test"
error_histogram_dir = "/Users/bulut/timenorm-garage/relation-extraction/egoitz-output-error-analysis/"
combined = {} # saves text, annotations and predictions of a single text file in each key

for text_path, text, xml_path, data in iter_data(original_dir, "gold"):
        text_file_name = text_path.split('/')[-1]
        combined[text_file_name] = {"text": text, "annotation": data}


for prediction_xml_path, prediction in iter_data_predictions(prediction_dir):
    prediction_xml_file_name = prediction_xml_path.split('/')[-2]
    if prediction_xml_file_name == ".DS_Store":
        continue
    combined[prediction_xml_file_name]["prediction"] = prediction

errors_histogram = {"missed":{}, "added":{}}
for k,v in combined.items():
    print("filename: ", k)
    ind_file_combined = {}
    for entity in v["annotation"].annotations:
        entity_id = entity.xml.find('id').text.split('@')[0]
        entity_type = entity.xml.find('type').text
        ind_file_combined[entity_id] = {"entity_type":entity_type}

    for entity in v["prediction"].annotations:
        entity_id = entity.xml.find('id').text.split('@')[0]
        ind_file_combined[entity_id]["predictions"] = []

        for prop in entity.properties.xml.iter():
            relation = prop.tag
            related_entity = prop.text
            if type(related_entity) == str and '@' in related_entity:
                related_entity_id = related_entity.split('@')[0]
                related_entity_type = ind_file_combined[related_entity_id]["entity_type"]
                ind_file_combined[entity_id]["predictions"].append({"relation":relation, "related_entity_id":related_entity_id, "related_entity_type":related_entity_type})
        
    for entity in v["annotation"].annotations:
        entity_id = entity.xml.find('id').text.split('@')[0]
        ind_file_combined[entity_id]["annotations"] = []

        for prop in entity.properties.xml.iter():
            relation = prop.tag
            related_entity = prop.text
            if type(related_entity) == str and '@' in related_entity:
                related_entity_id = related_entity.split('@')[0]
                try:
                    related_entity_type = ind_file_combined[related_entity_id]["entity_type"]
                except KeyError:
                    print("key error")
                    continue
                ind_file_combined[entity_id]["annotations"].append({"relation":relation, "related_entity_id":related_entity_id,  "related_entity_type":related_entity_type})


    for k,v in ind_file_combined.items():
        list_annotations = v["annotations"]
        list_predictions = v["predictions"]
        histogram_key = ""
        for annotation in list_annotations:
            if annotation not in list_predictions:
                histogram_key = ""
                histogram_key += annotation["relation"] + ":" + annotation["related_entity_type"]
                if histogram_key not in errors_histogram["missed"].keys():
                    errors_histogram["missed"][histogram_key] = 1
                else:
                    errors_histogram["missed"][histogram_key] += 1
                
        for prediction in list_predictions:
            if prediction not in list_annotations:
                histogram_key = ""
                histogram_key += prediction["relation"] + ":" + prediction["related_entity_type"]
                if histogram_key not in errors_histogram["added"].keys():
                    errors_histogram["added"][histogram_key] = 1
                else:
                    errors_histogram["added"][histogram_key] += 1
                histogram_key = ""


errors_histogram["missed"] = sort_dictionary(errors_histogram["missed"])
errors_histogram["added"] = sort_dictionary(errors_histogram["added"])
print(errors_histogram["missed"])
print("\n\n\n")
print(errors_histogram["added"])

combinations = list(errors_histogram["missed"].keys())
values = list(errors_histogram["missed"].values())
  
fig = plt.figure(figsize = (15, 10))
 
# creating the bar plots 
plt.bar(combinations, values, width = 0.15, color ='maroon', align='edge')
 
ax = plt.gca()
ax.set_xticklabels(combinations, rotation=40, ha="right")

plt.xticks(rotation=45)
plt.xlabel("missed relation:missed related entity type")
plt.ylabel("# of observations")
plt.yticks(np.arange(min(values), max(values)+1, 1.0))

plt.title("# of missed observations by combination of error")
plt.tight_layout()
plt.savefig(os.path.join(error_histogram_dir, "missed_histogram.png"), dpi=400, pad_inches=0)

plt.clf()
combinations = list(errors_histogram["added"].keys())
values = list(errors_histogram["added"].values())
  
fig = plt.figure(figsize = (15, 10))

plt.bar(combinations, values, width = 0.15, color ='maroon', align='edge')
 
ax = plt.gca()
ax.set_xticklabels(combinations, rotation=40, ha="right")

plt.xticks(rotation=45)
plt.xlabel("added relation:added related entity type")
plt.ylabel("# of observations")
plt.yticks(np.arange(min(values), max(values)+1, 1.0))

plt.title("# of added observations by combination of error")
plt.tight_layout()
plt.savefig(os.path.join(error_histogram_dir, "added_histogram.png"), dpi=400, pad_inches=0)



