import anafora

xml_path = "/Users/bulut/local-repositories/timenorm/src/main/python/utilTextFiles/schema.xml"

data = anafora.AnaforaData.from_file(xml_path)
entities = []

ctr = 0
for tag in data.xml:
    if tag.tag == "definition":
            for element in tag.findall("entities"):
                    for entity in element.findall("entity"):
                            entities.append(entity)

relations = ['Sub-Interval', 'Interval', 'Repeating-Interval', 'AMPM-Of-Day', 'Period', 'Number', 'Modifier', 'Every']

validity_dict_per_relation = {}
for relation in relations:
    validity_dict_per_relation[relation] = {
                'Period': [], 
                'Year': [], 
                'Calendar-Interval': [], 
                'Month-Of-Year': [], 
                'Day-Of-Month': [], 
                'Day-Of-Week': [], 
                'Hour-Of-Day': [], 
                'Minute-Of-Hour': [], 
                'Number': [], 
                'Second-Of-Minute': [], 
                'Time-Zone': [], 
                'Part-Of-Day': [], 
                'Season-Of-Year': [], 
                'AMPM-Of-Day': [], 
                'Part-Of-Week': [], 
                'Week-Of-Year': [], 
                'Two-Digit-Year': [], 
                'Sum': [], 
                'Difference': [], 
                'Union': [], 
                'Intersection': [], 
                'Every-Nth': [], 
                'This': [], 
                'Last': [], 
                'Next': [], 
                'Before': [], 
                'After': [], 
                'Between': [], 
                'NthFromStart': [],
                'NthFromEnd': [], 
                'Frequency': [], 
                'Modifier': [], 
                'Event': [], 
                'Quarter-Of-Year': [], 
                'PreAnnotation': [], 
                'NotNormalizable': [], 
                }
                
for k,v in validity_dict_per_relation.items():
    print(k)
    print(v)
    print("\n\n\n\n\n")


for entity in entities:
    entity_type = entity.get('type')
    print(entity_type)
    for prop in entity.iter():
        if prop.get('instanceOf'):
            relation = prop.get('type')
            print(relation)
            if relation in relations:
                related_types = prop.get('instanceOf').split(',')
                print(related_types)
                for related_type in related_types:
                    validity_dict_per_relation[relation][entity_type].append(related_type)


for k,v in validity_dict_per_relation.items():
    print(k)
    print(v)
    print("\n\n\n\n\n")

