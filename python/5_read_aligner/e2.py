#!/usr/local/bin/python
# The previous line (which must be the first one to work) makes the script self-executing,
# assuming that the system has the Python interpreter at path /usr/local/bin/python.

# This wants to be run in Python 3.


# Read Aligner
# Given: A string reference
#        A string that is the path to a file containing the pre-processed reference
#        A string that is the path to a file containing the reads
# Return: A list of locations from where the reads originated
# by Moyaccercchi

fo = open('in.txt', 'r')

reference = fo.readline().strip()
prepropath = fo.readline().strip()
readpath = fo.readline().strip()

fo.close()


suffix_array = []

fprepro = open(prepropath, 'r')
lastline = fprepro.readline().strip()

while (lastline != ""):
    lastline = lastline.split(" -> ")
    someval = lastline[1]
    somepos = someval.find(',')
    if somepos > -1:
        someval = someval[0:somepos-1]
    suffix_array.append((lastline[0], someval))
    lastline = fprepro.readline().strip()

fprepro.close()

suffix_array.sort(key=lambda pair: pair[0])

freads = open(readpath, 'r')


def findinarray(suffix_array, lastline):
    
    s = 0
    e = len(suffix_array)
    i = (s + e) // 2
    j = 1
    
    while j > 0:
        if suffix_array[i][0] < lastline:
            s = i
            i = (s + e) // 2
            j = i - s
        else:
            e = i
            i = (s + e) // 2
            j = e - i
    
    return suffix_array[i][1]


def align_reads(reference, suffix_array, freads):
    
    ret = []
    
    lastline = freads.readline().strip()
    
    while (lastline != ""):
        ret.append(findinarray(suffix_array, lastline))
        lastline = freads.readline().strip()
    
    return ret


def strarraytostring(arr):
    
    return "\n".join(arr)


res = strarraytostring(align_reads(reference, suffix_array, freads))

freads.close()

fo = open('out.txt', 'w')

fo.write(res)

fo.close()
