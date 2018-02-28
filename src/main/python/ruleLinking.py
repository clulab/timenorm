#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Sat Feb 18 18:50:43 2017

@author: egoitz
"""

import anafora_funct, text2num
import dateutil.parser as dprs

from lxml import etree
import os
import re
import sys
import configparser
import argparse




config = configparser.ConfigParser()
config.read('link.conf')
rawpath = config['DATA']['raw']
dctpath = config['DATA']['dct']
tn_schema = config['DATA']['schema']
date_types = config['DATA']['types']

parser = argparse.ArgumentParser(description='t4f-NN with domain adaptation.')
parser.add_argument('--input',
                    help='input directory.')
parser.add_argument('--output',
                    help='output directory.')
args = parser.parse_args()

path = args.input
out_path = args.output

tnschema = anafora_funct.get_schema(tn_schema)
types = anafora_funct.get_types(date_types)



def get_relation(tnschema, parent, child):
    if parent in tnschema:
        for relation in tnschema[parent]:
            if relation != "parentsType":
                for validChild in tnschema[parent][relation][1]:
                    if validChild == child:
                        return relation
    return ""


def process_doc(doc):
    for xmlfile in os.listdir(path + '/' + doc):
        axml = etree.parse(path + '/' + doc + '/' + xmlfile)
        rawfile = open(os.path.join(rawpath,  doc ), 'r')
        text = rawfile.read()
        rawfile.close()

        dctfile = open(os.path.join(dctpath, doc, doc + ".dct"), 'r')
        dct = dctfile.read().rstrip()
        dctfile.close()
        try:
            dct = dprs.parse(dct)
            dctDayofWeek = dct.strftime('%A')
        except ValueError:
            dctDayofWeek = ""

        entities = dict()
        starts = dict()
        for entity in axml.findall('.//entity'):
            eid = entity.find('./id').text
            estart, eend = map(int, entity.find('./span').text.split(','))
            etype = entity.find('./type').text
            eparentsType = entity.find('./parentsType')
            if eparentsType is not None:
                eparentsType = eparentsType.text
            else:
                eparentsType = tnschema[etype]["parentsType"]
                parentsType = etree.Element("parentsType")
                parentsType.text = eparentsType
                entity.append(parentsType)
            eproperties = entity.find('./properties')
            # Empty all links
            if eproperties is not None:
                for prop in eproperties.findall('./*'):
                    eproperties.remove(prop)
            else:
                prop = etree.Element("properties")
                entity.append(prop)
            if estart not in starts:
                starts[estart] = list()
            ent_values = (eid, estart, eend, etype, eparentsType)
            starts[estart].append(eid)
            entities[eid] = ent_values

        links = dict()
        stack = list()
        entity_list = dict()
        lend = -1
        for start in sorted(starts):
            for entity in starts[start]:
                (eid, estart, eend, etype, eparentsType) = entities[entity]
                if estart - lend > 10 and lend > -1:
                    stack = list()
                    entity_list = dict()
                lend = eend
                entity_list[eid] = (estart, eend, etype, eparentsType)
                ltype = ""
                stack_pointer = list()
                stack_pointer.extend(stack)
                while len(stack_pointer) > 0:
                    s = stack_pointer.pop()
                    stype = entity_list[s][2]
                    ltype = get_relation(tnschema, etype, stype)
                    if ltype != '':
                        if eid not in links:
                            links[eid] = dict()
                        if ltype not in links[eid]:
                            links[eid][ltype] = list()
                            links[eid][ltype].append(s)
                    else:
                        ltype = get_relation(tnschema, stype, etype)
                        if ltype != '':
                            if s not in links:
                                links[s] = dict()
                            if ltype not in links[s]:
                                links[s][ltype] = list()
                                links[s][ltype].append(eid)
                stack.append(eid)


        for entity in axml.findall('.//entity'):
            eid = entity.find('./id').text
            etype = entity.find('./type').text
            estart, eend = map(int, entity.find('./span').text.split(','))
            eproperties = entity.find('./properties')
            if etype in tnschema:
                for relation in tnschema[etype]:
                    if relation != "parentsType":
                        span = "".join(text[estart:eend])
                        if relation == "Type":
                            ptype = span.title()
                            if ptype == "About":
                                ptype = "Approx"
                            if etype in types:
                                if span in types[etype]:
                                    ptype = types[etype][span]
                            if etype == "Calendar-Interval" and ptype != "Unknown":
                                if ptype.endswith("s"):
                                    ptype = ptype[:-1]
                            elif etype == "Period" and ptype != "Unknown":
                                if not ptype.endswith("s"):
                                    ptype += "s"
                            ty = etree.Element(relation)
                            ty.text = ptype
                            eproperties.append(ty)
                        elif relation == "Value":
                            val = etree.Element(relation)
                            span = re.sub(r'^0(\d)', r'\1', re.sub(r'^0+', '0', span))
                            span = str(text2num.text2num(span))
                            val.text = span
                            eproperties.append(val)
                        elif re.search('Interval-Type',relation):
                            intervalemtpy = True
                            if eid in links:
                                if "Interval" in links[eid]:
                                    if links[eid]["Interval"] != "":
                                        intervalemtpy = False
                            if not intervalemtpy:
                                itype = etree.Element(relation)
                                itype.text = "Link"
                                eproperties.append(itype)
                            else:
                                itype = etree.Element(relation)
                                itype.text = "DocTime"
                                eproperties.append(itype)
                        elif relation == "Semantics":
                            sem = etree.Element(relation)
                            sem.text = "Interval-Not-Included"
                            eproperties.append(sem)
                        else:
                            notnull = False
                            if eid in links:
                                if relation in links[eid]:
                                    for child in links[eid][relation]:
                                        si = etree.Element(relation)
                                        si.text = child
                                        eproperties.append(si)
                                        notnull = True
                            if tnschema[etype][relation][0] and not notnull:
                                if eproperties.find('./' + relation) is None:
                                    si = etree.Element(relation)
                                    eproperties.append(si)
                if etype == "Last":
                    semantics = eproperties.findall('./Semantics')[0]
                    interval_included = "Interval-Not-Included"
                    for repint in eproperties.findall('./Repeating-Interval'):
                        if repint.text is not None:
                            (rid, rstart, rend, rtype, rparentsType) = entities[repint.text]
                            rspan = "".join(text[int(rstart):int(rend)])
                            if rspan.title() == dctDayofWeek:
                                interval_included = "Interval-Included"
                    semantics.text = interval_included

        if not os.path.exists(out_path + '/' + doc):
            os.makedirs(out_path + '/' + doc)
        axml.write(out_path + '/' + doc + '/' + xmlfile, pretty_print=True)


if __name__ == "__main__":
    for doc in os.listdir(path):
        if not doc.endswith(".txt") and not doc.endswith(".npy"):
            process_doc(doc)
