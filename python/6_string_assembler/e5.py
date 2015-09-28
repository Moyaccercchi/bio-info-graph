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
# by Moyaccercchi, 18th June 2015
# 
# version 5:
# - fixed off-by-one error for indels occuring after the anchor position (see trial 4)
# - improved indel behavior in general by converting conflicting indels into ones
#   that make sense automatically (see trial 5)

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
    
    rettmp = collections.defaultdict(lambda: [])
    retins = collections.defaultdict(lambda: 0)
    retdel = collections.defaultdict(lambda: 0)
    
    lastread = freads.readline().strip()
    lastalign = faligns.readline().strip()
    
    while (lastread != ""):

        # (analysis) print(lastread)

        if lastalign != "":

            # (analysis) print('(read is used)')
            
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
                    retdel[inf+curpos] += 1
                else:
                    if inf.find("ins at ") == 0:
                        inf = int(inf[7:])
                        retins[inf+curpos] += 1
            
            # We here NEED to allow several read at this position, and for several reasons:
            # (1) If we have several same reads at this position, they need to count as several
            #     reads later on, when we choose letters based on how many reads agree there.
            # (2) If we have several reads for which curpos+orientedat is the same, but
            #     curpos is different, then overwriting here actually means dropping real information!
            rettmp[curpos+orientedat].append([lastread, curpos])
        
        lastread = freads.readline().strip()
        lastalign = faligns.readline().strip()

    ret = collections.defaultdict(lambda: [])
    
    offset = 0
    
    # (analysis) print(retdel)
    # (analysis) print(retins)
    # (analysis) print(rettmp)
    
    # cater to the insertion / deletion madness
    
    # 120 percent of the reference length should be a far enough safety buffer
    for i in range(0, round(referenceLength * 1.2)):

        rdel = retdel[i]
        rins = retins[i]

        if (rdel > 0) and (rins > 0):
            if rdel > rins:
                rdel += rins
                rins = 0
            else:
                rins += rdel
                rdel = 0

        # here, a higher treshold could be chosen, ideally as percentage of reads at that position,
        # but in the current implementation we do not know that percentage at this code line
        if rdel > 0:
            offset -= 1
        if rins > 0:
            offset += 1
        
        for j in range(0, len(rettmp[i])):
            ret[rettmp[i][j][1] + offset].append(rettmp[i][j][0])
    
    # (analysis) print(ret);

    i = 0
    j = 0
    restr = ""
    carryonstr = ""
    carrystrs = [];
    qamount = 0;
    qtotal = 0;
    
    # iterate over all positions (plus the offset, so that we expand / retract if necessary)

    while (i < referenceLength + offset) or (i - j < len(carryonstr)):

        for j in range(0, len(ret[i])):
            carrystrs.append([i, ret[i][j]]);

        # get the one letter that each read aligned to this position has here

        allofthem = '';
        for k in range(0, len(carrystrs)):
            if (i - carrystrs[k][0] < len(carrystrs[k][1])):
                allofthem += carrystrs[k][1][i - carrystrs[k][0]]

        if (len(allofthem) > 0):

            # find the most common letters among allofthem

            c = collections.Counter(allofthem)

            mc = c.most_common()

            # if we have several most common letters, sort them alphabetically...

            sl = []
            hi = mc[0][1]
            for m in mc:
                if m[1] == hi:
                    sl.append(m[0])
                else:
                    break

            if (len(sl) > 1):
                sl.sort()

            # ... then take the first one
            # (so that if 3 reads say 'A' and 3 reads say 'T', we always choose 'A',
            # and never 'T', which makes it possible to compare our results
            # after more or less identical runs)

            r = sl[0]
            q = hi/len(allofthem)

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

    # (analysis) print(quality)
    # (analysis) print(restr)

    return str(quality) + "\n" + restr


res = assemble_string(referenceLength, freads, faligns, outputfiletype)

freads.close()

faligns.close()

fo = open('out.txt', 'w')

fo.write(res)

fo.close()
