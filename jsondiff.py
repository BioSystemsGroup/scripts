#!/usr/bin/python
# -*- coding: utf-8 -*-
import sys
f1n = sys.argv[1]
f2n = sys.argv[2]
print "Compare "+f1n+" "+f2n
 
import json
f1 = open(f1n)
f2 = open(f2n)
us1 = json.load(f1)
us2 = json.load(f2)
f1.close()
f2.close()
anydiffs = False
diffs = []
for key in us1:
    fv1 = us1[key]
    fv2 = us2[key]
    if fv1 != fv2:
        diffs.append(key)
        anydiffs = True
if anydiffs:
    print u"∃ diffs in", ", ".join(diffs)

    
exit
