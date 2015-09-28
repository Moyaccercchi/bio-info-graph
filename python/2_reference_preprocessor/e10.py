#!/usr/local/bin/python
# The previous line (which must be the first one to work) makes the script self-executing,
# assuming that the system has the Python interpreter at path /usr/local/bin/python.

# This wants to be run in Python 3.


# Reference Pre-Processor
# Given: A reference file
#        An integer horizon, which tells us how far to look ahead if we do look ahead
# Return: A suffix array (either with short strings, or with the BWT)
#
# by Moyaccercchi, 9th of May 2015
#
# version 10:
# added BWT support

import collections

fo = open('in.txt', 'r')

referencepath = fo.readline().strip()
horizon = int(fo.readline().strip())
useBWT = int(fo.readline().strip())

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



# we flatten a given reference string by choosing the first option from each bubble;
# however, we could instead also choose the second (or in fact a random) option,
# or ideally put N characters to indicate that there is a character,
# but it is unknown, so that the later steps do NOT count it as an error
# to align against it either way
# (of course, completely ideal would be to keep track of the graph itself,
# or in this case the bubbles, and use ALL the information later on in the chain,
# not ever discarding any of it)

# also, we are now having nested graphs, so we need to check if we are
# in fact encountering a nested one here, and do this recursively, which is
# why this is a function now rather than just a while iteration

def flattenreference(flat_reference):

    # find the first "(", so e.g. GA(GC(T|C)A|CAGG)T(G|A)C
    #                               ^

    i = flat_reference.find("(");

    if i > -1:

        # find what is before the first "(", so e.g. GA(GC(T|C)A|CAGG)T(G|A)C
        #                                            ^^

        flat_before = flat_reference[:i];

        # we would like to find what is the first alternative, so e.g.
        # GA(GC(T|C)A|CAGG)T(G|A)C
        #    ^^^^^^^^
        #
        # to do so, in version 8 of this script, we simply used
        # flat_first = flat_reference[i+1:flat_reference.find("|")];
        #
        # however, this is not sufficient anymore, as it would find the very first "|",
        # and in our example give us GA(GC(T|C)A|CAGG)T(G|A)C (where we are missing all ° chars)
        #                               ^^^^°°°°
        # so instead we first make a recursive call to this function itself, which will give us:
        # GC(T|C)A|CAGG)T(G|A)C => GCTA|CAGG)T(G|A)C => GCTA|CAGG)TGC
        
        flatter_reference = flattenreference(flat_reference[i+1:]);

        # flat_first is now the the start of the returned value, e.g.
        #
        # GCTA|CAGG)TGC = GA(GC(T|C)A|CAGG)T(G|A)C
        # ^^^^               ^^ ^   ^

        flat_first = flatter_reference[:flatter_reference.find("|")];

        # find what is after the ")" closing the first "(", so e.g.
        # GCTA|CAGG)TGC = GA(GC(T|C)A|CAGG)T(G|A)C
        #           ^^^                    ^ ^   ^

        flat_after = flatter_reference[flatter_reference.find(")")+1:];

        # take all the parts together to flatten one layer, so e.g.
        # (left is what is actually happening, right is what is conceptually happening)
        #
        # GA(GC(T|C)A|CAGG)T(G|A)C + GCTA|CAGG)TGC = GA(GC(T|C)A|CAGG)T(G|A)C
        # ^^                         ^^^^      ^^^   ^^ ^^ ^   ^      ^ ^   ^
        # GA                         GCTA      TGC = GA GC T   A      T G   C
        # =>                                         =>
        # GAGCTATGC                                = GAGCTATGC

        flat_reference = flat_before + flat_first + flat_after;

    return flat_reference;



def getbubblemidlen(reference, offset):

    # in v9, snipmidlen is more involved (it is the length of any path through the graph,
    # so we go for the first option always and keep track of that)

    curpos = offset + 1;
    nestedness = 0;

    snipmidlen = 0;

    addtomidlen = [];

    # A(GT(CT|TA)G|CAA(A|G)A)T
    #   ^^ ^^ xx ^ xxx x x x
    #
    # 0 11 22 22 1 111 2 2 1 0 nestedness
    #
    #   11 11 11 1 000 0 0 0   addtomidlen[0 = nestedness-1]
    #      11 00       1 0     addtomidlen[1 = nestedness-1]
    #   11 11 00 1 000 0 0 0   prod(addtomidlen) = addtomidlen[0] * .. * addtomidlen[nestedness - 1]

    while (nestedness > 0) or (reference[curpos] != '|'):
        
        if (reference[curpos] == '('):
            
            nestedness += 1
            addtomidlen.append(1)
        
        elif (reference[curpos] == ')'):
            
            nestedness -= 1
            del addtomidlen[-1]

        elif (reference[curpos] == '|'):
        
            addtomidlen[nestedness-1] = 0

        else:

            doadd = True;

            for atm in addtomidlen:
                if atm == 0:
                    doadd = False
                    break

            if doadd:
                snipmidlen += 1

        curpos += 1

    return snipmidlen


def getbubbleend(reference, offset):
    
    lr = len(reference)

    # in v9, snipend also is more involved (it is the end of the bubble,
    # but as bubbles can now be nested, it actually needs to keep track of
    # nesting levels while searching for the ending ")" that belongs to the opening "(",
    # where previously it could just be assumed that the first ")" belongs to the first "(")
    snipstarts = 1
    curloc = offset + 1

    while snipstarts > 0:
        nextstart = reference.find("(", curloc)
        nextend = reference.find(")", curloc)
        
        if nextstart == -1:
            nextstart = lr
        if nextstart < nextend:
            snipstarts += 1
            curloc = nextstart + 1
        else:
            snipstarts -= 1
            curloc = nextend + 1

    return curloc - 1



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

            # (multi-bubble, nested) graph reference in STPU


            # first, start by generating a changed out_reference in which we
            # replace bubbles by characters of equal length to keep a unified index system
            # (based on i_absolute)

            out_reference = flattenreference(out_reference);


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

                # version 8: for non-nested bubbles only
                # snipmidlen = reference.find('|', snipbeg + jumpedover) - (1 + snipbeg + jumpedover)
                # snipend = reference.find(")", snipbeg + jumpedover) - jumpedover

                # version 9: for nested bubbles too
                snipmidlen = getbubblemidlen(reference, snipbeg + jumpedover)
                snipend = getbubbleend(reference, snipbeg + jumpedover) - jumpedover


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
                #     [   ] <- read GAGA(, find '(' in last position, expand by 4
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


                # now let us have a look at reads containing several two-SNIPs:
                #
                # ...AGAGA(T|C)A(T|C)GAGA...
                #    [   ] <- read AGAGA without graph
                # 
                # ...AGAGA(T|C)A(T|C)GAGA...
                #     [   ] <- read GAGA(, find '(' in last position, expand by 4
                #     [       ] <- read GAGAT and GAGAC
                # 
                # ...AGAGA(T|C)A(T|C)GAGA...
                #      [       ] <- read AGATA and AGACA
                # 
                # ...AGAGA(T|C)A(T|C)GAGA...
                #       [       ] <- read GATA( and GACA(, find '(' in last position, expand by 4 more
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

                
                # And what about nested bubbles?
                #
                # ...AGAGA(T(T|C)|CG)AGAGA...
                #    [   ] <- read AGAGA without graph
                # 
                # ...AGAGA(T(T|C)|CG)AGAGA...
                #     [   ] <- read GAGA(, find '(' in last position, expand until the ')' that closes it
                #     [             ] <- read GAGATT, GAGATC and GAGACG,
                #                        reduce to GAGAT, GAGAT and GAGAC,
                #                        remove multiples to find GAGAT and GAGAC
                # 
                # ...AGAGA(T(T|C)|CG)AGAGA...
                #      [             ] <- read AGATTA, AGATCA and AGACGA,
                #                         reduce to AGATT, AGATC and AGACG, remove nothing
                # 
                # ...AGAGA(T(T|C)|CG)AGAGA...
                #       [             ] <- read GATTAG, GATCAG and GACGAG,
                #                          reduce to GATTA, GATCA and GACGA, remove nothing
                # 
                # ...AGAGA(T(T|C)|CG)AGAGA...
                #        [             ] <- read ATTAGA, ATCAGA and ACGAGA,
                #                           reduce to ATTAG, ATCAG and ACGAG, remove nothing
                # 
                # ...AGAGA(T(T|C)|CG)AGAGA...
                #         [             ] <- read TTAGAG, TCAGAG and CGAGAG,
                #                            reduce to TTAGA, TCAGA and CGAGA, remove nothing
                # 
                # find '(' in first read pos => jump over entire read,
                # fast-adding all reduced variants, that is
                # TTAGAG, TCAGAG and CGAGAG were given
                # TTAGA , TCAGA  and CGAGA  were already added
                #  TAGAG,  CAGAG and  GAGAG are added in the fast-adding round
                #
                #
                # ...AGAGA(T(T|C)|CG)AGAGA...
                #                    [   ] <- read AGAGA
                

                rf = reference[i:i+horizon]

                if rf[len(rf)-1] == '(':

                    # the last character of the read, which is where we encountered the '(' character
                    snipbeg = i+horizon-1

                    # in version 8, it was enough to do the following:
                    # snipmidlen = reference.find("|", snipbeg) - (1 + snipbeg);
                    # snipend = reference.find(")", snipbeg)

                    # however, now we have to also consider nested bubbles, so instead we do:
                    snipmidlen = getbubblemidlen(reference, snipbeg)
                    snipend = getbubbleend(reference, snipbeg)
                    
                    # again, this is the length of the snip - 1
                    sniplen = snipend - snipbeg
                    sniplens.append(sniplen)
                    snipmidlens.append(snipmidlen)

                    # (analysis) print ("\nnext:")
                    # (analysis) print ("snipbeg:")
                    # (analysis) print (snipbeg)
                    # (analysis) print ("snipmidlen:")
                    # (analysis) print (snipmidlen)
                    # (analysis) print ("snipend:")
                    # (analysis) print (snipend)
                    # (analysis) print ("sniplen:")
                    # (analysis) print (sniplen)
                    # (analysis) print ("rf old:")
                    # (analysis) print (rf)
                    # (analysis) print ("horizon old:")
                    # (analysis) print (horizon)

                    horizon += sniplen
                    rf = reference[i:i+horizon]

                    # (analysis) print ("rf new:")
                    # (analysis) print (rf)
                    # (analysis) print ("horizon new:")
                    # (analysis) print (horizon)

                if '(' in rf:

                    # this virtual read contains at least one SNIP
                    rfs = [rf]

                    while len(rfs) > 0:

                        rfsnew = []

                        for rfl in rfs:

                            # in version 8 it was enough to do
                            # snipbeg = rfl.find("(");

                            # however, we now have to consider nested bubbles as well

                            # we don't go for the first '(' that we encounter,
                            # but instead for the first '(' that belongs to an inner bubble,
                            # where an "inner bubble" is any bubble that contains no nested bubbles
                            #
                            # e.g. AG(CTAG|G(C(G|T)T|TAA))C(G|T), which represents
                            #      AG CTAG                C G
                            #      AG      G C G   T      C G
                            #      AG      G C   T T      C G
                            #      AG      G         TAA  C G
                            #      AG CTAG                C   T
                            #      AG      G C G   T      C   T
                            #      AG      G C   T T      C   T
                            #      AG      G         TAA  C   T
                            #
                            # in this example, there are two inner bubbles:
                            #      AG(CTAG|G(C(G|T)T|TAA))C(G|T)
                            #                 °°°°°        °°°°°
                            #
                            # More formally, a bubble is an inner bubble iff
                            # after its '(' a ')' follows sooner than another '('.

                            # (analysis) print('rfl', rfl)

                            snipbeg = rfl.find("(");

                            isInnerBubble = False;

                            while not isInnerBubble:

                                if snipbeg < 0:
                                    # no bubble? that's good enough!
                                    isInnerBubble = True
                                else:
                                    snipnextbeg = rfl.find("(", snipbeg+1)
                                    if snipnextbeg < 0:
                                        # no bubble start follows? then this is an inner one!
                                        isInnerBubble = True
                                    else:
                                        snipend = rfl.find(")", snipbeg)
                                        if snipend < snipnextbeg:
                                            # the end is nigh - nigher than the next opening, that is,
                                            # so this is an inner one!
                                            isInnerBubble = True
                                        else:
                                            # another round? then we should update snipbeg
                                            snipbeg = snipnextbeg

                            if snipbeg > -1:

                                snipmidlen = rfl.find("|", snipbeg) - (1 + snipbeg);
                                snipend = rfl.find(")", snipbeg);

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
                                if not ("," + str(i_absolute) + "," in "," + ret[rfl[:orighorizon]]):
                                    ret[rfl[:orighorizon]] += str(i_absolute) + ","

                                # position [1]:
                                # if a SNIP is in the first position, then add all possible reads
                                # for the following positions as well, all the way through the SNIP
                                if rf[0] == '(':
                                    # we start at 1, as we already added 0:orighorizon+0 two lines above
                                    for lc in range(1, snipmidlens[0]):
                                        # we here check if the location has already been returned for this
                                        # exact virtual read, which seems ridiculous, but actually makes sense,
                                        # considering the following example:
                                        # A(ACCCCCG|GCCCCCA)G
                                        # if we have a virtual read length of 5 (orighorizon == 5),
                                        # then when we get to 'CCCCC' in the second path, it has already
                                        # been added for the first path - and in exactly the same absolute location!
                                        if not ("," + str(i_absolute + lc) + "," in "," + ret[rfl[lc:orighorizon+lc]]):
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


def getAllCyclicStrings(instr):

    ret = []

    for i in range(0, len(instr)):
        ret.append(instr[i:len(instr)] + instr[0:i])

    return ret


if (useBWT):

    reference, out_reference = loadTextFromFile(referencepath)

    allCyclicStrings = getAllCyclicStrings(reference + '$')

    allCyclicStringsandIndicies = []

    for i in range(0, len(allCyclicStrings)):
        allCyclicStringsandIndicies.append([allCyclicStrings[i], i])

    allCyclicStringsandIndicies.sort()


    # Burrows-Wheeler Transform

    bwt = ''

    for i in range(0, len(allCyclicStringsandIndicies)):
        bwt += allCyclicStringsandIndicies[i][0][len(allCyclicStringsandIndicies[i][0]) - 1]

    fo = open('out.txt', 'w')

    fo.write(bwt)

    fo.close()
    

    # Suffix Array

    sa = ''

    for i in range(0, len(allCyclicStringsandIndicies)):
        sa += str(allCyclicStringsandIndicies[i][1]) + '\n'

    fo = open('sa.txt', 'w')

    fo.write(sa)

    fo.close()
    
else:

    res = dicttoadjacency(generate_suffix_array(referencepath, horizon))

    fo = open('out.txt', 'w')

    fo.write(res)

    fo.close()
