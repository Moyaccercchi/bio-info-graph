#!/usr/local/bin/python
# The previous line (which must be the first one to work) makes the script self-executing,
# assuming that the system has the Python interpreter at path /usr/local/bin/python.

# This wants to be run in Python 3.


# String Assembler
# Given: A string reference
#        A string that is the path to a file containing the reads
#        A string that is the path to a file containing the guesstimated read positions
# Return: An assembled string that is as close to the original individual string as possible
# 
# by Moyaccercchi, 24th Jan 2015
# 
# version 3:
# now also suited to use infostrings for indel-aware assembly

import collections

fo = open('in.txt', 'r')

reference = fo.readline().strip()
readpath = fo.readline().strip()
alignpath = fo.readline().strip()

fo.close()


freads = open(readpath, 'r')

faligns = open(alignpath, 'r')


def assemble_string(reference, freads, faligns):
    
    rettmp = collections.defaultdict(lambda: [0, ''])
    retins = collections.defaultdict(lambda: 0)
    retdel = collections.defaultdict(lambda: 0)
    
    lastread = freads.readline().strip()
    lastalign = faligns.readline().strip()
    
    while (lastread != ""):
        if lastalign != "":
            
            curpos = int(lastalign[1:lastalign.find(",")])
            orientedat = int(lastalign[lastalign.find(",")+2:lastalign.find("]")])
            
            infostr = lastalign[lastalign.find("]"):]
            if infostr != "":
                infostr = infostr[3:]
                
                while infostr.find(",") > -1:
                    inf = infostr[:infostr.find(",")]
                    
                    # analyse inf
                    if inf.find("del at ") == 0:
                        inf = int(inf[7:])
                        if inf < orientedat:
                            inf += 1
                        retdel[inf+curpos] += 1
                    else:
                        if inf.find("ins at ") == 0:
                            inf = int(inf[7:])
                            if inf < orientedat:
                                inf += 1
                            retins[inf+curpos] += 1
                    
                    infostr = infostr[infostr.find(",")+1:]
                
                inf = infostr[:infostr.find("]")]
                
                # analyse inf
                if inf.find("del at ") == 0:
                    inf = int(inf[7:])
                    if inf < orientedat:
                        inf += 1
                    retdel[inf+curpos] += 1
                else:
                    if inf.find("ins at ") == 0:
                        inf = int(inf[7:])
                        if inf < orientedat:
                            inf += 1
                        retins[inf+curpos] += 1
            
            rettmp[curpos+orientedat] = [lastread, curpos]
        
        lastread = freads.readline().strip()
        lastalign = faligns.readline().strip()
    
    ret = collections.defaultdict(lambda: '')
    
    offset = 0
    lenr = len(reference)
    
    print(retdel)
    print(retins)
    
    # 120 percent of the reference length should be a far enough safety buffer
    for i in range(0, round(lenr * 1.2)):
        if retdel[i] > 0:
            offset -= 1
        if retins[i] > 0:
            offset += 1
        
        if rettmp[i][1] != '':
            ret[rettmp[i][1] + offset] = rettmp[i][0]
    
    i = 0
    j = 0
    restr = ""
    carryonstr = ""
    
    while (i < lenr) or (i - j < len(carryonstr)):
        if ret[i] != "":
            restr += ret[i][0]
            carryonstr = ret[i]
            j = i
        else:
            if i - j < len(carryonstr):
                restr += carryonstr[i-j]
            else:
                restr += '_'
        
        i += 1
    
    # strip underline characters from the beginning
    i = 0
    lenr = len(restr)
    while (i < lenr) and (restr[i] == '_'):
        i += 1
    if i > 0:
        restr = restr[i:]
    
    # strip underline chracters from the end
    i = len(restr)-1
    while (i > -1) and (restr[i] == '_'):
        i -= 1
    if i < len(restr)-1:
        restr = restr[:i+1]
    
    return restr


res = assemble_string(reference, freads, faligns)

freads.close()

faligns.close()

fo = open('out.txt', 'w')

fo.write(res)

fo.close()
