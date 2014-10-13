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

import collections

fo = open('in.txt', 'r')

reference = fo.readline().strip()
prepropath = fo.readline().strip()
readpath = fo.readline().strip()

fo.close()


suffix_array = collections.defaultdict(lambda: '')

fprepro = open(prepropath, 'r')
lastline = fprepro.readline().strip()

while (lastline != ""):
    lastline = lastline.split(" -> ")
    suffix_array[lastline[0]] += lastline[1]
    lastline = fprepro.readline().strip()

fprepro.close()

freads = open(readpath, 'r')


def align_reads(reference, suffix_array, freads):
    
    ret = []
    
    lastline = freads.readline().strip()
    
    while (lastline != ""):
        ret.append(suffix_array[lastline])
        lastline = freads.readline().strip()
    
    return ret


def strarraytostring(arr):
    
    return "\n".join(arr)


res = strarraytostring(align_reads(reference, suffix_array, freads))

freads.close()

fo = open('out.txt', 'w')

fo.write(res)

fo.close()
