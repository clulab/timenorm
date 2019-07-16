#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Feb 18 18:50:43 2017

@author: egoitz
"""

import configparser

import anafora_funct

config = configparser.ConfigParser()
config.read('link.conf')
TN_SCHEMA = config['DATA']['schema']
TN_SCHEMA = anafora_funct.get_schema(TN_SCHEMA)
DATE_TYPES = config['DATA']['types']
DATE_TYPES = anafora_funct.get_types(DATE_TYPES)
EXCLUDED = config['OPTIONS']['excluded'].split(" ")