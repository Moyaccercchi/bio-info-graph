#!/usr/local/bin/python
# The previous line (which must be the first one to work) makes the script self-executing,
# assuming that the system has the Python interpreter at path /usr/local/bin/python.

# This wants to be run in Python 3.


# String Assembler
# Given: The length of a string reference
#        A string that is the path to a file containing the reads
#        A string that is the path to a file containing the guesstimated read positions
# Return: An assembled string that is as close to the original individual string as possible
# 
# by Moyaccercchi, 12th Apr 2015
# 
# version 4:
# now also suited to use infostrings for indel-aware assembly

import collections

fo = open('in.txt', 'r')

referenceLength = int(fo.readline().strip())
readpath = fo.readline().strip()
alignpath = fo.readline().strip()
outputfiletype = fo.readline().strip()

fo.close()


freads = open(readpath, 'r')

faligns = open(alignpath, 'r')


def assemble_string(referenceLength, freads, faligns, outputfiletype):
    
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
    
    # (analysis) print(retdel)
    # (analysis) print(retins)
    
    # 120 percent of the reference length should be a far enough safety buffer
    for i in range(0, round(referenceLength * 1.2)):
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
    carrystrs = [];
    qamount = 0;
    qtotal = 0;
    
    while (i < referenceLength) or (i - j < len(carryonstr)):

        if ret[i] != "":
            carrystrs.append([i, ret[i]]);
        
        # get the most common element from all aligned reads
        allofthem = '';
        for k in range(0, len(carrystrs)):
            if (i - carrystrs[k][0] < len(carrystrs[k][1])):
                allofthem += carrystrs[k][1][i - carrystrs[k][0]]

        if (len(allofthem) > 0):
            c = collections.Counter(allofthem)
            m = max(v for _, v in c.items())
            r = [k for k, v in c.items() if v == m][0]
            q = m/len(allofthem);
        else:
            r = '_'
            q = 0

        qtotal += q
        qamount += 1
        restr += r

        # (analysis) print(allofthem + ' C:' + r + ' Q: ', q)

        i += 1
    
    quality = qtotal / qamount

    # (analysis) print('quality: ', quality)

    # strip underline characters from the beginning
    i = 0
    lenr = len(restr)
    while (restr[i] == '_') and (i < lenr):
        i += 1
    if i > 0:
        restr = restr[i:]
    
    # strip underline characters from the end
    i = len(restr)-1
    while (restr[i] == '_') and (i > -1):
        i -= 1
    if i < len(restr)-1:
        restr = restr[:i+1]
    
    if outputfiletype == 'fasta':
        # add comment line in the beginning
        outtext = '>ASMBLD - assembled individual sequence, String Assembler by Moyaccercchi';

        # split sequence up into lines, each 70 characters long
        maxlen = len(restr);
        i = 0;
        while i < maxlen:
            outtext = outtext + "\n" + restr[i:i+70]
            i = i + 70
        
        restr = outtext

    return str(quality) + "\n" + restr


res = assemble_string(referenceLength, freads, faligns, outputfiletype)

freads.close()

faligns.close()

fo = open('out.txt', 'w')

fo.write(res)

fo.close()
