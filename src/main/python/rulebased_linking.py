#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Feb 18 18:50:43 2017

@author: egoitz
"""

import os
import re
import argparse
from anafora import walk
from lxml import etree
import dateutil.parser as dprs

from property_rules import complete_properties
import linking_conf as conf


class Entity(object):
    '''
    This class is used to store support information about the entity.
    '''
    def __init__(self, id, start, end, span, type, parentsType, properties):
        self.id = id
        self.start = start
        self.end = end
        self.span = span
        self.type = type
        self.parentsType = parentsType
        self.properties = properties
        self.targets = []
        self.sources = []


def filter_between_relations(relations, between, entity):
    start_indicators = ["from", "since"]
    end_indicators = ["to", "until"]
    fitered_relations = relations.copy()
    if 'End-Interval' in fitered_relations and between.start > entity.start:
        del fitered_relations['End-Interval']
    else:
        if ('Start-Interval' in fitered_relations and
                (between.properties.find('./Start-Interval')is not None or between.span.lower() in end_indicators)):
            del fitered_relations['Start-Interval']
        if ('End-Interval' in fitered_relations and
                (between.properties.find('./End-Interval')is not None or between.span.lower() in start_indicators)):
            del fitered_relations['End-Interval']
    return fitered_relations


def get_relation(parent, child):
    '''
    Returns the valid relation between parent and child.
    :param parent Entity: The parent entity.
    :param child Entity: The child entity.
    :return string: The relation type. Empty string if noun found.
    '''
    valid_relations = []
    if parent.type in conf.TN_SCHEMA:
        relations = conf.TN_SCHEMA[parent.type]
        if parent.type == "Between":
            relations = filter_between_relations(relations, parent, child)
        for relation in relations:
            if (relation != "parenType" and parent.properties.find(relation) is None and
                    parent.type not in conf.EXCLUDED and child.type not in conf.EXCLUDED):
                for validChild in conf.TN_SCHEMA[parent.type][relation][1]:
                    if validChild == child.type:
                        valid_relations.append(relation)
    return valid_relations


def get_entity(entity_list, entity_id):
    '''
    Look for an Entity in entity_list given its id
    :param entity_list list Entity: a list of Entity
    :param entity_id string: the target entity id
    :return Entity: the Entity if its found, None otherwise
    '''
    for entity in entity_list:
        if entity.id == entity_id:
            return entity
    return None


def get_and_prepare_entities(xml_tree, text):
    '''
    Get the identified entities and create a list of Entity.
    Add the parentsType if it doesn't exist. Empty the properties.
    :param xml_tree etree: a xml file with the entities identified
    :param text string: the original raw text
    :return entities list Entity: a list of Entity
    '''
    entities = list()
    for entity in xml_tree.findall('.//entity'):
        entity_id = entity.find('./id').text
        entity_start, entity_end = map(int, entity.find('./span').text.split(','))
        entity_span = "".join(text[entity_start:entity_end])
        entity_type = entity.find('./type').text
        entity_parentsType = entity.find('./parentsType')
        # Add the parentsType
        if entity_parentsType is not None:
            entity_parentsType = entity_parentsType.text
        else:
            entity_parentsType = conf.TN_SCHEMA[entity_type]["parentsType"]
            parentsType = etree.Element("parentsType")
            parentsType.text = entity_parentsType
            entity.append(parentsType)
        # Empty properties
        entity_properties = entity.find('./properties')
        if entity_properties is not None:
            for entity_property in entity_properties.findall('./*'):
                entity_properties.remove(entity_property)
        else:
            properties = etree.Element("properties")
            entity.append(properties)
        entities.append(Entity(entity_id, entity_start, entity_end, entity_span, entity_type,
                               entity_parentsType, entity.find('./properties')))
    return entities


def recursive_link(entity_list, target, source):
    '''
    Check if linking source to target will create a recursive link in the entity list
    :param entity_list list Entity: the list where to perform the search
    :param target Entity: the target Entity
    :param source Entity: the source Entity
    :return bool: True if the link will create a recursive link, False otherwise
    '''
    for property in target.properties:
        if source.id == property.text:
            return True
        property_entity = get_entity(entity_list, property.text)
        if recursive_link(entity_list, property_entity, source):
            return True
    return False


def type_linked(links, entity_type):
    '''
    Check if a type has been already linked to an entity
    to avoid revisiting it.
    In the case of "Day-Of-Week" or "Day-Of-Month" types
    take both into account.
    :param links list string:
    :param entity_type string:
    '''
    types_to_check = set([entity_type])
    if entity_type.startswith("Day-Of"):
        types_to_check.add("Day-Of-Week")
        types_to_check.add("Day-Of-Month")
    for type_to_check in types_to_check:
        if type_to_check in links:
            return True
    return False


def update_entity_links(parent_entity, child_entity, link_type):
    '''
    If link has beend found between two entities
    update the link list of both and create the
    property in the parent.
    :param parent_entity Entity:
    :param child_entity Entity:
    :param link_type string:
    '''
    parent_entity.targets.append(child_entity.type)
    child_entity.sources.append(parent_entity.type)
    link_value = child_entity.id
    new_link = etree.Element(link_type)
    new_link.text = link_value
    parent_entity.properties.append(new_link)


def try_link(entity_list, source, target):
    '''
    Try to link source to target given entity_list
    :param entity_list list Entity: the entity list where the link will be added
    :param source Entity: the source Entity
    :param target Entity: the target Entity
    :return bool: return True is success, False otherwise
    '''
    link_types = get_relation(source, target)
    linked = False
    if bool(link_types) and not recursive_link(entity_list, target, source):
        for link_type in link_types:
            if source.properties.find(link_type) is None:
                update_entity_links(source, target, link_type)
                linked = True
    return linked


def link_entities(entities, text):
    '''
    Link the entities together
    :param entities list Entity: list of Entity
    :param text string: the original raw text
    '''
    stack = list()
    previous = None
    for entity in sorted(entities, key=lambda e: e.start):
        # Emtpy the stack if there are 10 characters between previous entity and current entity
        # or both entities are in different sentences.
        new_sentence_rgx = r'[\.\!\?]*\s[A-Z]'
        within_span = "".join(text[previous.end:entity.start]) if previous is not None else ""
        if previous is not None and entity.start - previous.end > 10 or re.search(new_sentence_rgx, within_span):
            stack = list()
        previous = entity
        for pointer_entity in reversed(stack):
            if not bool(entity.sources) and not type_linked(entity.targets + entity.sources, pointer_entity.type):
                if not try_link(stack, entity, pointer_entity):
                    try_link(stack, pointer_entity, entity)
        stack.append(entity)


def process_documents(xml_paths, dct_paths, raw_paths, out_paths):
    '''
    Process documents in xml_paths using docTimes in dct_paths and raw text from raw_paths
    Store the results in out_paths files
    :param xml_paths list string: list with the xml file paths
    :param dct_paths list string: list with the dct file paths
    :param raw_paths list string: list with the raw file paths
    :param out_paths list string: list with the output file paths
    '''
    for xml_file_path, dct_file_path, raw_file_path, out_file_path in zip(xml_paths, dct_paths, raw_paths, out_paths):
        xml_tree = etree.parse(xml_file_path)

        with open(raw_file_path, 'r') as raw_file:
            text = raw_file.read()

        dctDayofWeek = None
        if dct_file_path is not None:
            with open(dct_file_path, 'r') as dct_file:
                dct = dct_file.read().rstrip()
            try:
                dct = dprs.parse(dct)
                dctDayofWeek = dct.strftime('%A') # This will be used for the "Semantics" of the "Last" operator
            except ValueError:
                pass

        entities = get_and_prepare_entities(xml_tree, text)
        link_entities(entities, text)
        complete_properties(entities, text, xml_tree, dctDayofWeek)

        os.makedirs(os.path.dirname(out_file_path), exist_ok=True)
        xml_tree.write(out_file_path, pretty_print=True)


def create_file_paths(dir_path, doc_names, extension):
    '''
    Return a list of file paths in dir_path/doc_names with extension.
    :param dir_path string: the parent directory
    :param doc_names list string: the list of document names
    :param extension string: the extension
    :return list string:
    '''
    out_paths = [os.path.join(dir_path, doc_name, doc_name + extension) for doc_name in doc_names]
    return out_paths


def set_paths(in_path, dct_path, raw_path, out_path, extension):
    xml_paths = walk(in_path, r"\.xml$")
    xml_paths = list(xml_paths)
    doc_names = [path[1] for path in xml_paths]
    xml_paths = [os.path.join(in_path, path[0], file_path)
                 for path in xml_paths for file_path in path[2]]
    if dct_path is None:
        dct_paths = [None] * len(doc_names)
    else:
        dct_paths = walk(dct_path, r"\.dct$")
        dct_paths = [os.path.join(in_path, path[0], file_path)
                     for path in dct_paths for file_path in path[2]
                     if path[1] in doc_names]
    if raw_path is None:
        raw_paths = [None] * len(doc_names)
    else:
        raw_paths = walk(raw_path, r"(\.txt|[^.]{4}$)")
        raw_paths = [os.path.join(in_path, path[0], file_path)
                     for path in raw_paths for file_path in path[2]
                     if path[1] in doc_names]
    out_paths = create_file_paths(out_path, doc_names, extension)
    return xml_paths, dct_paths, raw_paths, out_paths


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Rule based linking and property completion.')
    parser.add_argument('--input', required=True,
                        help='input directory.')
    parser.add_argument('--dct',
                        help='dct directory.')
    parser.add_argument('--raw', required=True,
                        help='raw directory.')
    parser.add_argument('--output', required=True,
                        help='output directory.')
    parser.add_argument('--ext', default='.TimeNorm.predicted.withlinks.xml',
                        help='extension for the output files. (default: %(default)s)')
    args = parser.parse_args()
    in_path = args.input
    out_path = args.output
    dct_path = args.dct
    raw_path = args.raw
    extension = args.ext

    xml_paths, dct_paths, raw_paths, out_paths = set_paths(in_path, dct_path, raw_path, out_path, extension)
    process_documents(xml_paths, dct_paths, raw_paths, out_paths)
