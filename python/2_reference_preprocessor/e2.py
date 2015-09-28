#!/usr/local/bin/python
# The previous line (which must be the first one to work) makes the script self-executing,
# assuming that the system has the Python interpreter at path /usr/local/bin/python.

# This wants to be run in Python 3.


# Reference Pre-Processor
# Given: A string reference
#        An integer horizon, which tells us how far to look ahead if we do look ahead
# Return: A suffix array
#
# by Moyaccercchi, 19th of Apr 2015
#
# version 2:
# allowing two-SNIPs for non-nested graphs as long as they are not so close to each other
# that several two-SNIPs appear in the same virtual read

import collections

fo = open('in.txt', 'r')

referencepath = fo.readline().strip()
horizon = int(fo.readline().strip())

fo.close()


def loadTextFromFile(filepath):
    
    fhandle = open(filepath, 'r')
    
    lastread = fhandle.readline().strip()
    
    if (filepath[-6:].lower() == '.fasta'):
        # We ignore the first read, as it is a comment line.
        res = ''
        while (lastread != ""):
            lastread = fhandle.readline().strip()
            res += lastread
    else:
        res = lastread
    
    fhandle.close()
    
    return res


def generate_suffix_array(referencepath, horizon):
    
    ret = collections.defaultdict(lambda: '')
    
    reference = loadTextFromFile(referencepath)
    
    if (referencepath[-6:].lower() == '.fasta'):

        # string reference in FASTA

        for i in range(0, len(reference)):
            ret[reference[i:i+horizon]] += str(i) + ","
    
    else:

        if '(' in reference:
            # (two-SNIP, non-nested) graph reference in STPU

            i = 0;
            lr = len(reference);

            while i < lr:

                # ATTENTION!
                # So far, this assumes that there only ever is one two-SNIP per read;
                # having multiple two-SNIPs in the same read requires an a bit more
                # elaborate approach. =)

                # imagine the following reads as [ ] and the following reference:
                #
                # ...AGAGA(T|C)AGAGA...
                #    [   ] <- read AGAGA without graph
                # 
                # ...AGAGA(T|C)AGAGA...
                #     [   ] <- read GAGA, find '(' in last position, expand by 4
                #     [       ] <- read GAGAT and GAGAC
                # 
                # ...AGAGA(T|C)AGAGA...
                #      [       ] <- read AGATA and AGACA
                # 
                # ...AGAGA(T|C)AGAGA...
                #       [       ] <- read GATAG and GACAG
                # 
                # ...AGAGA(T|C)AGAGA...
                #        [       ] <- read ATAGA and ACAGA
                # 
                # ...AGAGA(T|C)AGAGA...
                #         [       ] <- read TAGAG and CAGAG
                # 
                # instead of i+1, do i+5 (or i+4 and then i+1 due to the loop)
                # also, unexpand
                #
                # ...AGAGA(T|C)AGAGA...
                #              [   ] <- read AGAGA, unexpanded
                # 
                
                rf = reference[i:i+horizon]

                if rf[len(rf)-1] == '(':
                    horizon += 4
                    rf = reference[i:i+horizon]

                if '(' in rf:

                    rfs = [];

                    grStart = rf.find("(", 0,  len(rf));

                    rfs.append(rf[0:grStart] + rf[grStart+1] + rf[grStart+5:len(rf)]);
                    rfs.append(rf[0:grStart] + rf[grStart+3] + rf[grStart+5:len(rf)]);

                    for rfline in rfs:
                        ret[rfline] += str(i) + ","
                
                else:
                    ret[rf] += str(i) + ","

                if rf[0] == '(':
                    horizon -= 4
                    i += 4

                i += 1

        else:

            # string reference in STPU

            for i in range(0, len(reference)):
                ret[reference[i:i+horizon]] += str(i) + ","

    return ret


def dicttoadjacency(ourdict):
    
    ret = []
    
    for fromnode, tonode in ourdict.items():
        ret.append(fromnode + ' -> ' + tonode[:-1])
    
    return '\n'.join(sorted(ret))


res = dicttoadjacency(generate_suffix_array(referencepath, horizon))

fo = open('out.txt', 'w')

fo.write(res)

fo.close()
