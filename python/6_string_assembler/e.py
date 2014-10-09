#!/usr/local/bin/python
# The previous line (which must be the first one to work) makes the script self-executing,
# assuming that the system has the Python interpreter at path /usr/local/bin/python.

# This wants to be run in Python 2 or 3.


# String Assembler
# Given: A string reference
#        A string that is the path to a file containing the reads
#        A string that is the path to a file containing the guesstimated read positions
# Return: An assembled string that is as close to the original individual string as possible
# by Moyaccercchi

import collections

fo = open('in.txt', 'r')

reference = fo.readline().strip()
readpath = fo.readline().strip()
alignpath = fo.readline().strip()

fo.close()


freads = open(readpath, 'r')

faligns = open(alignpath, 'r')


def assemble_string(reference, freads, faligns):
    
    ret = collections.defaultdict(lambda: '')
    
    lastread = freads.readline().strip()
    lastalign = faligns.readline().strip()
    
    while (lastread != ""):
        if lastalign != "":
            ret[int(lastalign)] = lastread
        lastread = freads.readline().strip()
        lastalign = faligns.readline().strip()
    
    restr = ""
    j = 0
    carryonstr = ""
    
    for i in range(0, len(reference)):
        if ret[i] != "":
            restr += ret[i][0]
            carryonstr = ret[i]
            j = i
        else:
            if i - j < len(carryonstr) != "":
                restr += carryonstr[i-j]
            else:
                restr += '_'
    
    return restr


res = assemble_string(reference, freads, faligns)

freads.close()

faligns.close()

fo = open('out.txt', 'w')

fo.write(res)

fo.close()
