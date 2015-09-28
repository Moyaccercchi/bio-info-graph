#!/usr/local/bin/python
# The previous line (which must be the first one to work) makes the script self-executing,
# assuming that the system has the Python interpreter at path /usr/local/bin/python.

# This wants to be run in Python 3.


# Reference Pre-Processor
# Given: A reference file
#        An integer horizon, which tells us how far to look ahead if we do look ahead
# Return: A suffix array
#
# by Moyaccercchi, 22th of Apr 2015
#
# version 8:
#
# allows two-SNIPs for non-nested graphs even for several two-SNIPs for a virtual read
#
# also uses i_absolute to have an absolute counting mechanism for the later alignment
# (however, this only really works with SNIPs in which each option has the exact same
# size, so A(CG|GT|T(A|G))T should be the most complicated thing that still works with
# this method, while A(C|)T should be the least complicated thing that requires a
# completely different method - and by "works with" I mean being theoretically able
# to use that method this this type of reference, not that the current source code
# actually supports it)
#
# rewrites the reference for the following stages by transforming SNIPs
# into taking the first choice (which again only really makes sense if the SNIP
# options all have the same length), e.g. A(G|T)C becomes AGC, so that the numbering
# with i_absolute actually corresponds to the reference that the other steps further
# down the line will encounter
#
# further than that, also allows multi-SNIPs (SNIPs with more than two alternatives)
# as well as general same-length SNIPs (instead of just length 1, all SNIP lengths
# smaller than the horizon are supported - the "smaller than the horizon" part
# might still be necessary, I actually have not tried it out yet!)
#
# also, we are here fixing a bug from the previous versions;
# basically, what happens is that when the window is moved all the way to the SNIP
# starting at the left-most position, then the window jumps ahead over the SNIP
# (which makes sense, as it would be very hard to trace the SNIP when not having
# its start within the current window)
# this however means that virtual reads starting in a position within the SNIP different
# than the first of each path were not created, which is now solved by explicitly creating
# them (see position [1])

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
        origres = lastread

        while (lastread != ""):
            lastread = fhandle.readline().strip()
            res += lastread
            origres += '\n' + lastread

    else:

        # STPU files
        res = lastread
        origres = lastread
    
    fhandle.close()
    
    return (res, origres)


def generate_suffix_array(referencepath, horizon):

    orighorizon = horizon
    
    ret = collections.defaultdict(lambda: '')
    
    reference, out_reference = loadTextFromFile(referencepath)

    if (referencepath[-6:].lower() == '.fasta'):

        # string reference in FASTA

        for i in range(0, len(reference)):
            ret[reference[i:i+horizon]] += str(i) + ","
    
    else:

        if '(' in reference:

            # (multi-SNIP, non-nested) graph reference in STPU


            # first, start by generating a changed out_reference in which we
            # replace SNIPs by characters of equal length to keep a unified index system
            # (based on i_absolute)

            i = out_reference.find("(", 0, len(out_reference));

            while i > -1:

                # here we are just choosing the first option from the multi-SNIP;
                # however, we could instead also choose the second (or in fact a random) option,
                # or ideally put N characters to indicate that there is a character,
                # but it is unknown, so that the later steps do NOT count it as an error
                # to align against it either way
                # (of course, completely ideal would be to keep track of the graph itself,
                # or in this case the SNIP, and use ALL the information later on in the chain,
                # not ever discarding any of it)

                out_reference = out_reference[:i] + \
                                out_reference[i+1:out_reference.find("|", 0, len(out_reference))] + \
                                out_reference[out_reference.find(")", 0, len(out_reference))+1:];

                i = out_reference.find("(", 0, len(out_reference));


            # actually do the preprocessing of the reference

            i = 0;
            i_absolute = 0;
            lr = len(reference)

            # keeps track of the sniplengths we have encountered, to be able to counteract them later
            sniplens = []
            snipmidlens = []

            # initiate the horizon by adding the length of the SNIP - 1 to it for each '(' before
            # the last character of the first read
            
            rf = reference[0:horizon-1]
            jumpedover = 0
            
            while '(' in rf:
                
                snipbeg = rf.find("(")
                snipmidlen = reference.find('|', snipbeg + jumpedover) - (1 + snipbeg + jumpedover)
                snipend = reference.find(")", snipbeg + jumpedover) - jumpedover

                # this is not the real length of the snip, but rather the length - 1
                sniplen = snipend - snipbeg

                sniplens.append(sniplen)
                snipmidlens.append(snipmidlen)
                
                if (snipend+1 > len(rf)):
                    rf = reference[snipend+horizon-len(rf):horizon+sniplen-1]
                else:
                    rf = rf[snipend+1:] + reference[horizon-1:horizon+sniplen-1]
                
                jumpedover += snipend+1
                
                horizon += sniplen

            # to get virtual reads all the way to the end, go for:
            # while i < lr:

            # to only get virtual reads of the full specified length, go for:
            while i < lr-horizon+1:

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

                    # the last character of the read, which is where we encountered the '(' character
                    snipbeg = i+horizon-1
                    snipmidlen = reference.find("|", snipbeg) - (1 + snipbeg);
                    snipend = reference.find(")", snipbeg)

                    # again, this is the length of the snip - 1
                    sniplen = snipend - snipbeg
                    sniplens.append(sniplen)
                    snipmidlens.append(snipmidlen)

                    print ("\nnext:")
                    print ("snipbeg:")
                    print (snipbeg)
                    print ("snipmidlen:")
                    print (snipmidlen)
                    print ("snipend:")
                    print (snipend)
                    print ("sniplen:")
                    print (sniplen)
                    print ("rf old:")
                    print (rf)
                    print ("horizon old:")
                    print (horizon)

                    horizon += sniplen
                    rf = reference[i:i+horizon]

                    print ("rf new:")
                    print (rf)
                    print ("horizon new:")
                    print (horizon)

                if '(' in rf:

                    # this virtual read contains at least one SNIP
                    rfs = [rf]

                    while len(rfs) > 0:

                        rfsnew = []

                        for rfl in rfs:

                            snipbeg = rfl.find("(");
                            snipmidlen = rfl.find("|", snipbeg) - (1 + snipbeg);
                            snipend = rfl.find(")", snipbeg);

                            if snipbeg > -1:

                                # we add the virtual read with the first choice and with the
                                # second choice for the first SNIP in it, e.g.
                                # AG(C|T)G(C|T)A -> AGCG(C|T)A, AGTG(C|T)A

                                beforesnip = rfl[:snipbeg]
                                aftersnip = rfl[snipend+1:len(rfl)]

                                snipstart = snipbeg+1;
                                while (snipstart < snipend):
                                    rfsnew.append(beforesnip + rfl[snipstart:snipstart+snipmidlen] + aftersnip)
                                    snipstart += snipmidlen + 1

                            else:

                                # all SNIPs have been replaced by choices from them
                                ret[rfl[:orighorizon]] += str(i_absolute) + ","

                                # position [1]:
                                # if a SNIP is in the first position, then add all possible reads
                                # for the following positions as well, all the way through the SNIP
                                if rf[0] == '(':
                                    # we start at 1, as we already added 0:orighorizon+0 two lines above
                                    for lc in range(1, snipmidlens[0]):
                                        ret[rfl[lc:orighorizon+lc]] += str(i_absolute + lc) + ","

                        rfs = rfsnew[:]
                
                else:

                    # this virtual read contains no SNIP, so just use it purely
                    ret[rf[:orighorizon]] += str(i_absolute) + ","

                if rf[0] == '(':
                    sniplen = sniplens.pop(0)
                    snipmidlen = snipmidlens.pop(0)
                    if (sniplen < 0):
                        sniplen = 0
                    horizon -= sniplen

                    i += sniplen + 1
                    i_absolute += snipmidlen

                else:

                    i += 1
                    i_absolute += 1

        else:

            # string reference in STPU

            for i in range(0, len(reference)):
                ret[reference[i:i+horizon]] += str(i) + ","

    fref = open('outref.txt', 'w')

    fref.write(out_reference)

    fref.close()

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
