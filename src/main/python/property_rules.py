#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Feb 18 18:50:43 2017

@author: egoitz
"""

import re
from lxml import etree

import text2num
import linking_conf as conf

def Type(entity):
    '''
    Rules for "Type" property
    :param entity Entity:
    :return property_value string:
    '''
    property_value = entity.span.title()
    if entity.type in conf.DATE_TYPES:
        if entity.span in conf.DATE_TYPES[entity.type]:
            property_value = conf.DATE_TYPES[entity.type][entity.span]
        # Type of "Calendar-Interval" must be singular
        if entity.type == "Calendar-Interval" and property_value != "Unknown":
            if property_value.endswith("s"):
                property_value = property_value[:-1]
        # Type of "Period" must be plural
        elif entity.type == "Period" and property_value != "Unknown":
            if not property_value.endswith("s"):
                property_value += "s"
    return property_value


def Value(entity):
    '''
    Rules for "Value" property
    :param entity Entity:
    :return property_value string:
    '''
    property_value = re.sub(r'^0+', '0', entity.span)
    property_value = re.sub(r'^0(\d)', r'\1', property_value)
    property_value = re.sub(r'(\d)(st|nd|rd|th)$', r'\1', property_value)
    try:
        property_value = str(text2num.text2num(property_value))
    except text2num.NumberException:
        pass
    return property_value


def Interval_Type(entity):
    '''
    Rules for "Interval-Type", "Interval-Type-Start" and "Interval-Type-End" properties
    :param entity Entity:
    :return property_value string:
    '''
    if entity.properties.xpath("./Interval[./text()]"):
        return "Link"
    else:
        return "DocTime"


def Semantics(entity=None, xml_tree=None, text=None, dctDayofWeek=None):
    '''
    Rules for "Semantics" property.
    For newswire style "Last" operator, if the operator is
    linked to a "Repeating-Interval" equal to the docTime "Day-Of-Week"
    the "Semantics" of the operator must be set to "Interval-Included"
    :param entity Entity:
    :param xml_tree etree:
    :param text string:
    :param dctDayofWeek string:
    :return property_value string:
    '''
    if entity.type == "Last" and dctDayofWeek is not None:
        for repeating_interval in entity.properties.findall('./Repeating-Interval'):
            if repeating_interval.text is not None:
                for repeating_interval_entity in xml_tree.xpath('//entity[./id="' + repeating_interval.text + '"]'):
                    rep_int_start, rep_int_end = map(int, repeating_interval_entity.find('./span').text.split(','))
                    repeating_interval_span = "".join(text[int(rep_int_start):int(rep_int_end)])
                    if repeating_interval_span.title() == dctDayofWeek:
                        return "Interval-Included"
    return "Interval-Not-Included"


def complete_properties(entities, text, xml_tree, dctDayofWeek):
    '''
    Complete the entity properties that are not links
    :param entities:
    :param text:
    :param xml_tree:
    :param dctDayofWeek:
    '''
    for entity in entities:
        if entity.type in conf.TN_SCHEMA:
            for property_type in conf.TN_SCHEMA[entity.type]:
                if property_type != "parentsType":
                    property_value = None

                    # Call the rules for the property type
                    if property_type == "Type":
                        property_value = Type(entity)
                    elif property_type == "Value":
                        property_value = Value(entity)
                    elif re.search("Interval-Type", property_type):
                        property_value = Interval_Type(entity)
                    elif property_type == "Semantics":
                        property_value = Semantics(entity, xml_tree, text, dctDayofWeek)

                    # Add the property if the value has been solved
                    if property_value is not None:
                        new_property = etree.Element(property_type)
                        new_property.text = property_value
                        entity.properties.append(new_property)
                    # If there is no value for the property_type add <none>
                    elif conf.TN_SCHEMA[entity.type][property_type][0] and entity.properties.find(
                            './' + property_type) is None:
                        new_property = etree.Element(property_type)
                        entity.properties.append(new_property)
