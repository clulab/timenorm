import anafora
import argparse
import os
import re
from collections import Counter


def countDuplicate(data, key):

    cnt = Counter()

    for entity in data.annotations:
        # exclude the "event" type
        if entity.type == 'Event':
            continue
        
        if key in entity.properties:
            spans_type = (entity.spans, entity.type)
            cnt[spans_type] += 1
    
    cnt = { x: count for x, count in cnt.items() if count > 1 }
    
    return cnt

def getDuplicateMap(data_dict):

    dup_dict = {}

    for path, data in data_dict.items():

        cnt = countDuplicate(data, 'Sub-Interval')
    
        if len(cnt.items()) == 0: pass
        else: dup_dict[path] = cnt

    return dup_dict


def findBetween(data_dict):
    properties = set()
    with open('./set.txt', 'w+') as fp:
        for path, data in data_dict.items():
            
            for entity in data.annotations: 
                if entity.type == 'Between':
                    
                    for p in enumerate(entity.properties):
                        if p[1] not in properties:
                            properties.add(p[1])
                            
            fp.write(path + ' ' + str(properties) + '\n')
    
    return         


def sub_to_super(input_dir, output_dir):
    paths = anafora.walk(input_dir, xml_name_regex=r'TimeNorm\.gold\.completed')
    for sub_dir, text_file_name, xml_file_names in paths:
        for xml_file_name in xml_file_names:
            input_path = os.path.join(input_dir, sub_dir, xml_file_name)
            data = anafora.AnaforaData.from_file(input_path)
            for entity in data.annotations:
                if 'Sub-Interval' in entity.properties:
                    sub_entity = entity.properties['Sub-Interval']
                    if sub_entity:
                        sub_entity.properties['Super-Interval'] = entity.id
                    del entity.properties['Sub-Interval']
            output_parent = os.path.join(output_dir, sub_dir)
            if not os.path.exists(output_parent):
                os.makedirs(output_parent)
            data.to_file(os.path.join(output_parent, xml_file_name))


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("input_dir")
    parser.add_argument("output_dir")
    args = parser.parse_args()

    sub_to_super(**vars(args))
