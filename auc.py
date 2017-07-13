#!/usr/bin/python -d
#
# This calculates the AUC (area under the curve) for a given data
# set using the trapezoidal rule.
#
import sys
import string

def error(e):
    """Take the error info and echo usage info to user."""
    print "Error: %s." % e
    print "Usage: " + sys.argv[0] + " <csv file>"
    sys.exit(-1)

if len(sys.argv) == 1:
    error("Please provide a csv file.")

datfilename = sys.argv[1]

datfile = file(datfilename,"r")
dat = {}

# read header
line = datfile.readline()
try:
    float(string.split(line,',')[0])
except:
    head = string.split(line,',')
    print "I'm seeing %d columns." % (len(head)-1)
    for i in head[0:len(head)-1]:
        dat[i] = []
else:
    error("Please include a comma separated header on the first line.")

while 1:
    line = datfile.readline()
    if line == "":
        break
    try:
        floats = string.split(line,',')
        if (len(floats)-1 != len(head)-1):
            error("Number of data columns (%d) doesn't match number of headers (%d)." % (len(floats)-1, len(head)-1))
        # the "-1" below is for the stupid extra column in our data
        count = 0
        for i in head:
            dat[i].append(float(floats[count]))
            count = count + 1
    except SystemExit: # sys.exit(-1) really just throws an exception
        sys.exit(-1)
    except:
        continue

datfile.close()

# initialize the auc values for all the columns
auc = {}
for i in head[1:]:
    auc[i] = 0.0

index = head[0]
for h in head[1:len(head)-1]:
    
    auctemp = 0.0
    for j in (range(len(dat[index])-1)):
        area = (dat[h][j] + dat[h][j+1])*(dat[index][j+1]-dat[index][j])/2
        auctemp = auctemp + area

    auc[h] = auctemp

# write out the data
print "AUC"
for h in head[1:len(head)-1]:
    sys.stdout.write(" AUC(%s) = %f\n" % (h, auc[h]))
sys.stdout.write("\n")

sys.exit(0)
