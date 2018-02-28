"""
Created on Sat Feb 18 18:50:43 2017

@author: egoitz; dongfang
"""


import sys
import os
from lxml import etree
import dateutil.parser as dprs
import datetime
import numpy as np
from numpy import random


n_files = 800
path = "data/self-created/syn2"

if sys.platform == "linux" or sys.platform == "linux2":
    dtformats = [("%-m/%-d/%y", "%Y/%-d/%B"), #7/24/17
                 #("%A, %B %d, %Y", "%A-%B-%d-%Y"), #Monday, July 24, 2017
                 ("%m/%d/%y", "%B/%-d/%Y"), #07/24/17
                 ("%m/%d/%Y", "%B/%-d/%Y"), #07/24/2017
                 ("%b %-d, %y", "%B/%-d/%Y"), #Jul 24, 17
                 ("%b %-d, %Y", "%B/%-d/%Y"), #Jul 24, 2017
                 ("%-d. %-b. %Y", "%-d/%B/%Y"), #24. Jul. 2017
                 ("%B %-d, %Y", "%B/%-d/%Y"), #July 24, 2017
                 ("%-d. %B %Y", "%-d/%B/%Y"), #24. July 2017
                 #("%a, %b %-d, %y", "%A-%B-%d-%Y"), #Mon, Jul 24, 17
                 #("%a %d/%b %y", "%A-%d-%B-%Y"), #Mon 24/Jul 17
                 #("%a, %B %-d, %Y", "%A-%B-%d-%Y"), #Mon, July 24, 2017
                 #("%A, %B %-d, %Y", "%A-%B-%d-%Y"), #Monday, July 24, 2017
                 ("%m-%d", "%B/%-d"), #07-24
                 ("%y-%m-%d", "%Y/%B/%-d"), #17-07-24
                 ("%Y-%m-%d", "%Y/%B/%-d"), #2017-07-24
                 ("%m/%d", "%B/%-d"), #07/24
                 ("%b %d", "%B/%-d"), #Jul 24
                 ("%m/%d/%y %H:%M %p", "%B/%-d/%Y/%-H/%-M/%p"), #07/24/17 09:52 AM
                 ("%m/%d/%Y %H:%M:%S", "%B/%-d/%Y/%-H/%-M/%-S"), #07/24/2017 09:52:57
                 ("%Y-%m-%d %H:%M:%S", "%Y/%B/%-d/%-H/%-M/%-S"), #2017-07-24 09:52:57
                 ]
    
    tokord = {"%Y" : 0,             ##Year with century as a decimal number.##
          "%B" : 1,                 ##Month as Local's full name.
          "%-d" : 2,                ##Day of the month as a zero-padded decimal number.
          "%-H" : 3,                ##Hour (24-hour clock) as a zero-padded decimal number.
          "%-M" : 4,                ##Minute as a zero-padded decimal number.
          "%-S" : 5,                ##Second as a zero-padded decimal number.
          "%p" : 6,                 ##Locale's equivalent of either am or pm
        } 
else:
    dtformats = [("%A, %B %#d, %Y", "%A/%B/%#d/%Y"),  #Monday, July 24, 2017
                 ("%a., %b. %#d, %y", "%A/%B/%#d/%Y"),  # Mon., Jul. 24, 17
                 ("%a, %b %#d, %y", "%A/%B/%#d/%Y"),  # Mon, Jul 24, 17
                 ("%a. %#d/%b. %y", "%A/%#d/%B/%Y"),  # Mon. 24/Jul. 17
                 ("%a. %#d/%B %y", "%A/%#d/%B/%Y"),  # Mon. 24/July 17
                 ("%a., %B-%#d, %Y", "%A/%B/%#d/%Y"),  # Mon., July-24, 2017
                 ("%a, %B-%#d, %Y", "%A/%B/%#d/%Y"),  # Mon, July-24, 2017
                 ("%A, %B-%d, %Y", "%A/%B/%#d/%Y"),  # Monday, July-24, 2017

                 ("%b. %#d, %y", "%B/%#d/%Y"),  # Jul. 24, 17
                 ("%b. %#d, %Y", "%B/%#d/%Y"),  # Jul. 24, 2017
                 ("%#d %b. %Y", "%#d/%B/%Y"),  # 24 Jul. 2017
                 ("%B %#d, %Y", "%B/%#d/%Y"),  # July 24, 2017
                 ("%#d %B %Y", "%#d/%B/%Y"),  # 24 July 2017

                 ("%m-%d", "%B/%#d"),  #07-24
                 ("%m/%d", "%B/%#d"),  # 07/24
                 ("%b. %d", "%B/%#d"),  # Jul. 24

                 ("%y-%m-%d", "%Y/%B/%#d"),  #17-07-24
                 ("%Y/%m/%d", "%Y/%B/%#d"),  #2017/07/24
                 ("%y/%m/%d", "%Y/%B/%#d"),  # 17/07/24
                 ("%Y-%m-%d", "%Y/%B/%#d"),  # 2017-07-24

                 ("%m/%d/%y", "%B/%#d/%Y"),  # 07/24/17
                 ("%m/%d/%Y", "%B/%#d/%Y"),  # 07/24/2017

                 ("%m/%d/%y %H:%M %p", "%B/%#d/%Y/%#H/%#M/%p"),  #07/24/17 09:52 AM
                 ("%m/%d/%Y %H:%M:%S", "%B/%#d/%Y/%#H/%#M/%#S"),  #07/24/2017 09:52:57
                 ("%Y-%m-%d %H:%M:%S", "%Y/%B/%#d/%#H/%#M/%#S"),  #2017-07-24 09:52:57

                 ("%Y%m%d", "%Y/%B/%#d"),  ##19980108         0,4  4,6 6,8
                 ("%y%m%d", "%Y/%B/%#d"),  ##980108         0,2   2,4  4,6
                 ("%m-%d-%y %H%M %p", "%B/%#d/%Y/%#H/%#M/%p"),  ##08-15-90 1337 PM     0,2      3,5  6,8  9,11  11,13  14,16
                 ("%y-%m-%d %H%M", "%Y/%B/%#d/%#H/%#M"),  ##90-08-15 1337   0,2      3,5  6,8  9,11  11,13
                 ("%Y-%m-%d %H%M", "%Y/%B/%#d/%#H/%#M"),  ##1990-08-15 1337  0,4  5,7  8,10  11,13  13,15
                 ("%m-%d %H%M %p", "%B/%#d/%#H/%#M/%p"),  ##08-15 1337 PM    0,2      3,5  6,8  8,10  11,13
                 ("%m/%d/%y %H:%M%p", "%B/%#d/%Y/%#H/%#M/%p"),  # 07/24/17 09:52AM
                 ("%m-%d %H%M", "%B/%#d/%#H/%#M")  ##08-15 1337   0,2      3,5  6,8  8,10              33
                 ]
    
    tokord = {"%Y" : 0,
          "%B" : 1,
          "%#d" : 2,
          "%#H" : 3,
          "%#M" : 4,
          "%#S" : 5,
          "%p" : 6,
          "%A":7,
        } 

    

types = [("Year", "Interval"),
         ("Month-Of-Year", "Repeating-Interval"),
         ("Day-Of-Month", "Repeating-Interval"),
         ("Hour-Of-Day", "Repeating-Interval"),
         ("Minute-Of-Hour", "Repeating-Interval"),
         ("Second-Of-Minute", "Repeating-Interval"),
         ("AMPM-Of-Day", "Repeating-Interval"),
         ("Day-Of-Week","Repeating-Interval")
         ]

#separator = ["-","/"," ",":",","]


for d in random.randint(0,high=1600000000,size=int(n_files)):
    date = datetime.datetime.fromtimestamp(d)
    docdate = date.strftime('%Y%m%d%H%M')
    doc = "".join(["randdate",docdate])

    f = random.randint(0,high=len(dtformats),size=1)[0]
    special = 24

    #for f in range(20,len(dtformats)):
    newdate = date.strftime(dtformats[f][0])
    spans = list()

    if f <= special:
        start = 0
        for c in range(0,len(newdate)):
            if newdate[c] == "-" or newdate[c] == "/" or newdate[c] == " " or newdate[c] == ":" or newdate[c] ==",":
                end = c
                if start != end:
                    spans.append((str(start),str(end)))
                start = c + 1
        end = len(newdate)
        spans.append((str(start),str(end)))
    elif f==special+1:   ##19980108         0,4  4,6 6,8
        spans = [('0','4'),('4','6'),('6','8')]
    elif f==special+2:   ##980108         0,2   2,4  4,6
        spans = [('0','2'),('2','4'),('4','6')]
    elif f ==special+3:   ##08-15-90 1337 PM     0,2      3,5  6,8  9,11  11,13  14,16
        spans = [('0','2'),('3','5'),('6','8'),('9','11'),('11','13'),('14','16')]
    elif f ==special+4:  ##90-08-15 1337   0,2     3,5  6,8  9,11  11,13
        spans = [('0', '2'), ('3','5'), ('6', '8'), ('9', '11'), ('11', '13')]
    elif f ==special+5:##1990-08-15 1337  0,4  5,7  8,10  11,13  13,15
        spans = [('0','4'),('5','7'),('8','10'),('11','13'),('13','15')]
    elif f ==special+6:   ##08-15 1337 PM    0,2      3,5  6,8  8,10  11,13
        spans = [('0','2'),('3','5'),('6','8'),('8','10'),('11','13')]
    elif f ==special+7:   # 07/24/17 09:52AM    0,2  3,5  6,8  9,11  12,14   14,16
        spans = [('0','2'),('3','5'),('6','8'),('9','11'),('12','14'),('14','16')]
    else:     ##08-15 1337   0,2      3,5  6,8  8,10
        spans = [('0', '2'), ('3', '5'), ('6', '8'), ('8', '10'), ('11', '13')]

    newvalues = date.strftime(dtformats[f][1])
    values = newvalues.split('/')
    order = [tokord[j] for j in dtformats[f][1].split("/")]
    sort_order = np.argsort(order)

    root = etree.Element("data")
    anno = etree.SubElement(root,"annotations")
    for e,o in enumerate(sort_order):
        s = spans[o]
        v = values[o]
        (t, p) = types[order[o]]
        ent = etree.SubElement(anno,"entity")
        eid = etree.SubElement(ent,"id")
        eid.text = str(e) + "@" + doc + "@auto"
        span = etree.SubElement(ent,"span")
        span.text = ",".join(s)
        etype = etree.SubElement(ent,"type")
        etype.text = t

    if not os.path.exists(path + '/' + doc):
        os.makedirs(path + '/' + doc)
    et = etree.ElementTree(root)
    et.write(path + '/' + doc + '/' + doc + '.xml', pretty_print=True, xml_declaration=True,   encoding="utf-8")
    textfile = open(path + '/' + doc + '/' + doc,'w')
    textfile.write(newdate + '\n')
    textfile.close()
    
    