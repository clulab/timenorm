import anafora
import argparse
import os
from collections import Counter

def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("--dir", default=".",type=str, dest="path")

    return parser.parse_args()

def xml_to_data(root):
    # to stroe the file that has duplicate ids.
    duplicate_list = []
    
    # the map of xml file name to it's duplicate counter
    data_dict = {}
    
    for sub_dir, _, xml_names in anafora.walk(root, xml_name_regex='TimeNorm\.gold\.completed'):
        for xml in xml_names:
            # check only the TimeNorm.gold.completed files
            # if 'TimeNorm.gold.completed' in xml:
            xml_path = root + '/' + sub_dir + '/'  + xml
            try: 
                data = anafora.AnaforaData.from_file(xml_path)
                data_dict[xml_path] = data
            except:
                duplicate_list.append(xml_path)
    
    return data_dict

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

def deleteTag(entity):
    try:
        sub = entity.properties['Sub-Interval']
        entity.properties.__delitem__('Sub-Interval')
    except:
        pass

def sanityCheck(data):

    with open('./test_entities_super.txt', 'a') as fp:
        
        anno = data.annotations
        
        for entity in anno:

            if 'Super-Interval' in entity.properties._tag_to_property_xml.keys():
                
                sup = entity.properties['Super-Interval']
                
                if sup == '':
                    fp.write(entity.id + '->' + 'None' + '\n')
                    
                else:
                    fp.write(entity.id + '->' + sup.id + '\n')  

def modifyTag(data_dict):
  
    for path, data in data_dict.items():
        
        modifiedData = anafora.AnaforaData()
        
        anno = data.annotations
        
        for entity in anno:

            if 'Sub-Interval' in entity.properties:
                
                sub = entity.properties['Sub-Interval']
                if sub:
                    if isinstance(sub, str):
                        with open('./error.txt', 'a') as fp:
                            fp.write(path + ':\n')
                            fp.write(sub + '\n\n')
                    else:
                        sub.properties['Super-Interval'] = entity.id
                        entity.properties['Super-Interval'] = ''
                        # this delete tag would keep the errro ids
                        deleteTag(entity)
                else:
                    entity.properties.__delitem__('Sub-Interval')

    
            modifiedData.annotations.append(entity)

        out_path = './test_out/'
        paths = path.split('/')
        out_path += '/'.join(paths[1:-1])
        
        if not os.path.exists(out_path): 
            os.makedirs(out_path)
            
        modifiedData.to_file(out_path + f'/modified.gold.completed.xml')
        
        # sanityCheck(modifiedData)

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

if __name__ == "__main__":
    
    args = parse_args()
    DIR = args.path
    
    data_dict = xml_to_data(DIR)


    # dup_dict = getDuplicateMap(data_dict) 
    # with open('./duplicate.txt', 'w+') as fp:
    #     for key, value in dup_dict.items():
    #         fp.write(key + ':' + '\n')
    #         for span_type, count in value.items():
    #             fp.write(str(span_type) + str(count) + '\n')
    #         fp.write('\n')

    # modify the tags
    modifyTag(data_dict)


