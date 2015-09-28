#!/usr/local/bin/python
# The previous line (which must be the first one to work) makes the script self-executing,
# assuming that the system has the Python interpreter at path /usr/local/bin/python.

# This wants to be run in Python 3.


# Reference Pre-Processor
# Given: A reference file
#        An integer horizon, which tells us how far to look ahead if we do look ahead
# Return: A suffix array
#
# by Moyaccercchi, 19th of Apr 2015
#
# version 4:
# allows two-SNIPs for non-nested graphs even for several two-SNIPs for a virtual read
# and uses i_absolute to have an absolute counting mechanism for the later alignment
# (however, this only really works with SNIPs in which each option has the exact same
# size, so A(CG|GT|T(A|G))T should be the most complicated thing that still works with
# this method, while A(C|)T should be the least complicated thing that requires a
# completely different method - and by "works with" I mean being theoretically able
# to use that method this this type of reference, not that the current source code
# actually supports it)

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
            i_absolute = 0;
            lr = len(reference);

            # initiate the horizon by adding 4 to it for each '(' before
            # the last character of the first read
            rf = reference[0:horizon-1]
            while '(' in rf:
                rf = rf[rf.find("(", 0,  len(rf))+1:] + reference[horizon-1:horizon+3]
                horizon += 4

            while i < lr:

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

                # now let us have a look at reads containing several two-SNIPs:
                #
                # ...AGAGA(T|C)A(T|C)GAGA...
                #    [   ] <- read AGAGA without graph
                # 
                # ...AGAGA(T|C)A(T|C)GAGA...
                #     [   ] <- read GAGA, find '(' in last position, expand by 4
                #     [       ] <- read GAGAT and GAGAC
                # 
                # ...AGAGA(T|C)A(T|C)GAGA...
                #      [       ] <- read AGATA and AGACA
                # 
                # ...AGAGA(T|C)A(T|C)GAGA...
                #       [       ] <- read GATA and GACA, find '(' in last position, expand by 4 more
                #       [           ] <- read GATAT, GATAC, GACAT and GACAC
                # 
                # ...AGAGA(T|C)A(T|C)GAGA...
                #        [           ] <- read ATATG, ATACG, ACATG and ACACG
                # 
                # ...AGAGA(T|C)A(T|C)GAGA...
                #         [           ] <- read TATGA, TACGA, CATGA and CACGA
                # 
                # instead of i+1, do i+5 (or i+4 and then i+1 due to the loop)
                # also, unexpand
                #
                # ...AGAGA(T|C)A(T|C)GAGA...
                #              [       ] <- read ATGAG and ACGAG
                # 
                
                rf = reference[i:i+horizon]

                if rf[len(rf)-1] == '(':
                    horizon += 4
                    rf = reference[i:i+horizon]

                if '(' in rf:

                    # this virtual read contains at least one SNIP
                    rfs = [rf]

                    while len(rfs) > 0:

                        rfsnew = []

                        for rfl in rfs:
                            grStart = rfl.find("(", 0,  len(rfl));

                            if grStart > -1:
                                # we add the virtual read with the first choice and with the
                                # second choice for the first SNIP in it, e.g.
                                # AG(C|T)G(C|T)A -> AGCG(C|T)A, AGTG(C|T)A
                                rfsnew.append(rfl[0:grStart] + rfl[grStart+1] + rfl[grStart+5:len(rfl)])
                                rfsnew.append(rfl[0:grStart] + rfl[grStart+3] + rfl[grStart+5:len(rfl)])
                            else:
                                # all SNIPs have been replaced by choices from them
                                ret[rfl] += str(i_absolute) + ","

                        rfs = rfsnew[:]
                
                else:

                    # this virtual read contains no SNIP, so just use it purely
                    ret[rf] += str(i_absolute) + ","

                if rf[0] == '(':
                    horizon -= 4
                    i += 4

                i += 1
                i_absolute += 1

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
