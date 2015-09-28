#!/usr/local/bin/python
# The previous line (which must be the first one to work) makes the script self-executing,
# assuming that the system has the Python interpreter at path /usr/local/bin/python.

# This wants to be run in Python 3.


# Read Aligner
# Given: A string reference
#        A string that is the path to a file containing the pre-processed reference
#        A string that is the path to a file containing the reads
#        An integer that represents d, the maximum amount of mismatches for reads to be considered
#        An integer that represents plen, the exact length for each interval in the pigeonhole algorithm
#        (this could be calculated on the fly, but that would make the string-comparisons against the
#        suffix array more expensive, so we'll make it fixed-length instead - whoop whoop!)
# Return: A list of locations from where the reads originated
# 
# by Moyaccercchi, 25th June 2015
# 
# version 14:
# improving BWT support
# 
# the string "# (analysis) " can be globally replaced with an empty string to obtain debug output

import collections

fo = open('in.txt', 'r')

referencepath = fo.readline().strip()
prepropath = fo.readline().strip()
sapath = fo.readline().strip()
readpath = fo.readline().strip()
d = int(fo.readline().strip())
k = int(fo.readline().strip())
plen = int(fo.readline().strip())

# 0 .. use hashes with adjusted reference [new]
# 1 .. use BWT [similar Siren2014]
# 2 .. use hashes [similar to Schneeberger2009]
useBWT = int(fo.readline().strip())

usepigeonhole = int(fo.readline().strip())

fo.close()


freads = open(readpath, 'r')



# --------------------------------------------------------------------------------------------
#     General Helper Functions
# --------------------------------------------------------------------------------------------

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



# --------------------------------------------------------------------------------------------
#     Pigeonhole-Algorithm in general (used with and without Burrows-Wheeler Transform)
# --------------------------------------------------------------------------------------------

# We here match forward, that is, left to right, regular reading order
# Text is the reference string part to be analysed
# Pattern is the read string part to be analysed
# d is the amount of mismatches that is allowed
# j is the current offset at which we start
#   it is only used to form a more informative infostr
# abortat is used to avoid unnecessary recursion
#   (basically, when we have already found an answer that has a score of say 5,
#   then we don't need to consider any possibility for which the score is 4 or less
#   and can immediately abort that calculation)
def matchwithindelsfwd(Text, Pattern, d, j, abortat):
    
    # (analysis) print('fwd  text:    ' + Text);
    # (analysis) print('fwd  pattern: ' + Pattern);

    lenP = len(Pattern)
    
    for i in range(0, lenP):
        if (Text[i] != Pattern[i]):
            d -= 1
            if d < abortat:
                return [d, ""]
            
            # edits are most likely, so we try these first
            b,binf = matchwithindelsfwd(Text[i+1:lenP], Pattern[i+1:lenP], d, i+j+1, 0)

            # (analysis) print("fwd  edit: \n" + Text[i+1:lenP] + "\n" + Pattern[i+1:lenP])
            
            # indels are less likely, so we try them afterwards
            # ideally, we have at this stage already found a pretty solid
            # edit-solution and can simply skip when abortat is underrun
            a,ainf = matchwithindelsfwd(Text[i:lenP-1], Pattern[i+1:lenP], d, i+j+1, max(0, b))
            c,cinf = matchwithindelsfwd(Text[i+2:lenP], Pattern[i+1:lenP-1], d, i+j+1, max(0, a, b))
            
            # (analysis) print("fwd  insertion: \n" + Text[i:lenP-1] + "\n" + Pattern[i+1:lenP])
            # (analysis) print("fwd  deletion: \n" + Text[i+2:lenP] + "\n" + Pattern[i+1:lenP-1])
            
            ainf = "ins at " + str(i+j) + ", " + ainf
            cinf = "del at " + str(i+j) + ", " + cinf
            
            # (analysis) print("fwd  p: " + Pattern[i+1:lenP] + " a: " + str(a) + " b: " + str(b) + " c: " + str(c) + " i: " + str(i))
            
            if c > b:
                if c > a:
                    maxd = c
                    maxinf = cinf
                else:
                    maxd = a
                    maxinf = ainf
            else:
                if a > b:
                    maxd = a
                    maxinf = ainf
                else:
                    maxd = b
                    maxinf = binf
            
            # (analysis) print('fwd  maxd: ' + str(maxd) + ' maxinf: ' + maxinf)
            
            return [maxd, maxinf]
    
    return [d, ""]


# We here match backward, that is, right to left, inverse reading order
# Text is the reference string part to be analysed
# Pattern is the read string part to be analysed
# d is the amount of mismatches that is allowed
# j is the current offset at which we start
#   it is only used to form a more informative infostr
# abortat is used to avoid unnecessary recursion
#   (basically, when we have already found an answer that has a score of say 5,
#   then we don't need to consider any possibility for which the score is 4 or less
#   and can immediately abort that calculation)
def matchwithindelsbwd(Text, Pattern, d, abortat):
    
    # (analysis) print('bwd  text:    ' + Text);
    # (analysis) print('bwd  pattern: ' + Pattern);

    lenP = len(Pattern)
    
    if (len(Text) != len(Pattern)):
        return [d, ""]
    
    for i in range(lenP-1, -1, -1):
        if (Text[i] != Pattern[i]):
            d -= 1
            if d < abortat:
                return [d, ""]
            
            # edits are most likely, so we try these first
            b,binf = matchwithindelsbwd(Text[0:i], Pattern[0:i], d, 0)
            
            # indels are less likely, so we try them afterwards
            # ideally, we have at this stage already found a pretty solid
            # edit-solution and can simply skip when abortat is underrun
            a,ainf = matchwithindelsbwd(Text[1:i+1], Pattern[0:i], d, max(0, b))
            c,cinf = matchwithindelsbwd(Text[0:i], Pattern[1:i+1], d, max(0, a, b))
            
            ainf = "ins at " + str(i+1) + ", " + ainf
            cinf = "del at " + str(i+1) + ", " + cinf
            
            # (analysis) print("bwd  p: " + Pattern[0:i] + " a: " + str(a) + " b: " + str(b) + " c: " + str(c) + " i: " + str(i))
            
            if c > b:
                if c > a:
                    maxd = c
                    maxinf = cinf
                else:
                    maxd = a
                    maxinf = ainf
            else:
                if a > b:
                    maxd = a
                    maxinf = ainf
                else:
                    maxd = b
                    maxinf = binf

            # (analysis) print('bwd  maxd: ' + str(maxd) + ' maxinf: ' + maxinf)
            
            return [maxd, maxinf]
    
    return [d, ""]


# check if a Pattern is a match with at most d mismatches at a given position in the text
# 
# here, position is actually an array with two components:
#   first, the position of the read within the text, and
#   second the position of a completely matching part within that read
# 
# returns (1, score, info) if Pattern is a match,
#   where score is d minus the amount of mismatches found
#   (that is, the higher the score, the fewer mismatches are there)
#   info is a string containing extra information on the type of match
#   (e.g. a split match due to some indel)
# returns (0, 0, "") otherwise
# 
def isamatch(Pattern, Text, d, position, plen):

    # (analysis) print('im   p: ' + Pattern + ' p[0]: ' + str(position[0]) + ' p[1]: ' + str(position[1]));
    
    intextpos = position[0]
    inreadpos = position[1]
    lenP = len(Pattern)
    
    while intextpos < 0:
        Text = "_" + Text
        intextpos += 1
    
    lenT = len(Text)
    
    while intextpos + lenP > lenT:
        Text += "_"
        lenT += 1
    
    # (analysis) print(Text[intextpos:intextpos+len(Pattern)])
    # (analysis) print(Pattern)
    # (analysis) nuf = ""
    # (analysis) for i in range(0, inreadpos):
    # (analysis)     nuf += " "
    # (analysis) print(nuf + Pattern[inreadpos:inreadpos+plen])

    # (analysis) print('imb  ' + Text[intextpos:intextpos+inreadpos] + ' ' + Pattern[0:inreadpos])
    # (analysis) print('imf  ' + Text[intextpos+inreadpos+plen:intextpos+lenP] + ' ' + Pattern[inreadpos+plen:lenP])
    
    # eliminate strings that have too many mismatches - before aligned part
    d,infbwd = matchwithindelsbwd(Text[intextpos:intextpos+inreadpos], Pattern[0:inreadpos], d, 0)
    if d < 0:
        # (analysis) print("no alignment")
        return [0, 0, ""]
    
    # eliminate strings that have too many mismatches - after aligned part
    j = inreadpos+plen+1
    d,inffwd = matchwithindelsfwd(Text[intextpos+inreadpos+plen:intextpos+lenP], Pattern[inreadpos+plen:lenP], d, j, 0)
    if d < 0:
        # (analysis) print("no alignment")
        return [0, 0, ""]
    
    infostr = infbwd + inffwd
    infostr = infostr[0:len(infostr)-2]
    
    # (analysis) print("yes! alignment! [" + infostr + "]")
    return [1, d, infostr]



# --------------------------------------------------------------------------------------------
#     Pigeonhole-Algorithm without Burrows-Wheeler Transform
# --------------------------------------------------------------------------------------------

# call the pigeonhole algorithm
# Pattern .. the read that we currently analyse
# Text .. the reference string
# d .. maximum amount of mismatches
# k .. added to the maximum amount of mismatches for quick parts of the algorithm
# suffix_array .. the pre-processed reference as suffix array
# plen .. length for parts of the pigeonhole algorithm, that is, roughly len(Pattern) / (d+1)
def pigeonhole(Pattern, Text, d, k, suffix_array, plen):

    # (analysis) print("pigeonhole, plen = ", plen, ", plen should be = ", len(Pattern) / (d+1))
    
    # (analysis) print("\n\n" + Pattern + "\n")
    
    j = 0
    lenP = len(Pattern)
    
    # we have one more part than mismatches are allowed,
    # e.g. for 2 allowed mismatches we consider 3 parts,
    # such that 1 part MUST be present entirely correctly
    parts = d + 1
    
    # in possible_ret we store arrays where the first number
    # indicates the position at which the read itself starts
    # and the second number indicates the position at which
    # the part of the read that really does align start within
    # the read
    possible_ret = []
    
    # we first of all search for all the possible positions -
    # that is, positions where the suffix array exactly fits the reads
    
    for i in range(1, parts+1):
        newj = j + plen
        if newj > lenP:
            newj = lenP
            j = newj - plen
        
        # now Pattern[j:newj] is the part of the read that we want to align
        # correctly (that is, without any mismatches at all)

        sufarr = suffix_array[Pattern[j:newj]]
        
        # (analysis) print("j: " + str(j) + " newj: " + str(newj) + " pat: " + Pattern[j:newj] + " suf: " + str(sufarr))
        
        if len(sufarr) > 0:
            lastfind = sufarr.find(',')
            while lastfind > -1:
                possible_ret.append([int(sufarr[0:sufarr.find(',')]) - j, j])
                sufarr = sufarr[sufarr.find(',')+1:]
                lastfind = sufarr.find(',')
            possible_ret.append([int(sufarr) - j, j])
        j = newj
    
    # convert to set to eliminate double entries, back to list, then sort
    # possible_ret = sorted(list(set(possible_ret)))
    # actually, for version 6, let's not eliminate double entries -
    # this way we can analyse each match on its own, indel-wise
    
    ret = []
    
    # now all our candidate positions are ready, we merely need to check
    # if we have at most d mismatches for strings at these positions

    # in theory we should be able to re-use some information within the
    # following for-loop, but in practice this is hard due to the up to
    # d mismatches that we would need to keep track of
    
    for i in range(0, len(possible_ret)):
        
        # (analysis) print("possible_ret[%d]: (%d, %d)" % (i, possible_ret[i][0], possible_ret[i][1]))

        # we actually allow for d+k mismatches for improved performance;
        # that is, as mismatches often appear next to each other, we might
        # be lucky and several mismatches fall into the same interval such
        # that we don't need to go up with the main d, but only the sub-d
        # here - increasing this d here by 3 leads to significantly improved
        # results (from 73% correct assembly up to 96% correct assembly)
        # while still being a lot less expensive than increasing the main d
        # even by just 1
        ismatch,scoreofmatch,infostr = isamatch(Pattern, Text, d+k, possible_ret[i], plen)
        if ismatch:
            if len(infostr) > 0:
                infostr = " [" + infostr + "]"
            ret.append([scoreofmatch, possible_ret[i], infostr])
    
    return ret



# --------------------------------------------------------------------------------------------
#     Pigeonhole-Algorithm with Burrows-Wheeler Transform
# --------------------------------------------------------------------------------------------

# call the pigeonhole algorithm
# Pattern .. the read that we currently analyse
# Text .. the reference string
# d .. maximum amount of mismatches
# k .. added to the maximum amount of mismatches for quick parts of the algorithm
# suffix_array .. plain suffix array
# BWT .. the Burrows-Wheeler Transform
# numBWT .. the indices of the Burrows-Wheeler Transform
# firstCol .. the first column (sorted BWT)
# numfirstCol .. the indices of the first column
# plen .. length for parts of the pigeonhole algorithm, that is, roughly len(Pattern) / (d+1)
def pigeonholeBWT(Pattern, Text, d, k, suffix_array, BWT, numBWT, firstCol, numfirstCol, plen, C, alphabetToNum, numToAlphabet, use_advanced_BWT):

    # (analysis) print("\npigeonhole, plen = ", plen, ", plen should be = ", len(Pattern) / (d+1), ' pattern is ', Pattern, ' text is ', Text)
    
    # (analysis) print("\n\n" + Pattern + "\n")
    
    j = 0
    lenP = len(Pattern)
    
    # we have one more part than mismatches are allowed,
    # e.g. for 2 allowed mismatches we consider 3 parts,
    # such that 1 part MUST be present entirely correctly
    parts = d + 1
    
    # in possible_ret we store arrays where the first number
    # indicates the position at which the read itself starts
    # and the second number indicates the position at which
    # the part of the read that really does align start within
    # the read
    possible_ret = []
    
    # we first of all search for all the possible positions -
    # that is, positions where the suffix array exactly fits the reads
    
    for i in range(1, parts+1):
        newj = j + plen
        if newj > lenP:
            newj = lenP
            j = newj - plen
        
        # now Pattern[j:newj] is the part of the read that we want to align
        # correctly (that is, without any mismatches at all)

        [firstRow, lastRow] = findBestRows(Pattern[j:newj], BWT, numBWT, firstCol, numfirstCol, C, alphabetToNum, numToAlphabet, use_advanced_BWT);

        # (analysis) print('first: ', firstRow, ' last: ', lastRow)
        # (analysis) print("j: " + str(j) + " newj: " + str(newj) + " pat: " + Pattern[j:newj])
        
        if lastRow > -1:
            for currow in range(firstRow, lastRow+1):
                # (analysis) print('lookat: ', currow, suffix_array[currow], suffix_array[currow] - j, j)
                possible_ret.append([suffix_array[currow] - j, j])
        j = newj
    
    # (analysis) print('investigating...')
    # (analysis) print(possible_ret)

    # convert to set to eliminate double entries, back to list, then sort
    # possible_ret = sorted(list(set(possible_ret)))
    # actually, for version 6, let's not eliminate double entries -
    # this way we can analyse each match on its own, indel-wise
    
    ret = []
    
    # now all our candidate positions are ready, we merely need to check
    # if we have at most d mismatches for strings at these positions

    # in theory we should be able to re-use some information within the
    # following for-loop, but in practice this is hard due to the up to
    # d mismatches that we would need to keep track of
    
    for i in range(0, len(possible_ret)):
        
        # (analysis) print("\npossible_ret[%d]: (%d, %d)" % (i, possible_ret[i][0], possible_ret[i][1]))

        # we actually allow for d+k mismatches for improved performance;
        # that is, as mismatches often appear next to each other, we might
        # be lucky and several mismatches fall into the same interval such
        # that we don't need to go up with the main d, but only the sub-d
        # here - increasing this d here by 3 leads to significantly improved
        # results (from 73% correct assembly up to 96% correct assembly)
        # while still being a lot less expensive than increasing the main d
        # even by just 1
    
        ismatch,scoreofmatch,infostr = isamatch(Pattern, Text, d+k, possible_ret[i], plen)
        if ismatch:
            # we have an indel
            if len(infostr) > 0:
                infostr = " [" + infostr + "]"
                # reduce the score, as edits are more likely to happen than indels
                # (the number by which it is reduced here means that one indel is equivalent
                # to three edits - you may want to tweak this number!)
                scoreofmatch -= 2

            # (analysis) print('append ' + str(possible_ret[i]) + infostr + ' with score ' + str(scoreofmatch))

            ret.append([scoreofmatch, possible_ret[i], infostr])
    
    # (analysis) print("\nret: " + str(ret))

    return ret



# --------------------------------------------------------------------------------------------
#     Burrows-Wheeler Transform in general
# --------------------------------------------------------------------------------------------

def last_to_first(i, BWT, numBWT, firstCol, numfirstCol):

    targetLetter = BWT[i]
    targetNum = numBWT[i]

    for i in range(0, len(BWT)):
        if (firstCol[i] == targetLetter) and (numfirstCol[i] == targetNum):
            return i


# this function returns the first and last row on which the first column contains the given letter
def find_rows_with_letter(letter, BWT, startAt, endAt):

    startRow = -1;
    endRow = -1;

    for i in range(startAt, endAt+1):
        if BWT[i] == letter:
            startRow = i
            break

    for i in range(endAt, startAt-1, -1):
        if BWT[i] == letter:
            endRow = i
            break

    return [startRow, endRow]



# internally used for BWT with pigeonhole algorithm AND for BWT without pigeonhole algorithm
def findBestRows(lastline, BWT, numBWT, firstCol, numfirstCol, C, alphabetToNum, numToAlphabet, use_advanced_BWT):

    # as seen in Siren2014
    if (use_advanced_BWT):

        P = lastline
        lenP = len(P)

        [sp, ep] = [C[alphabetToNum[P[lenP-1]]], C[alphabetToNum[P[lenP-1]] + 1] - 1]
        # in the Siren2014 paper, sp and ep would each be one higher

        for i in range(lenP-2, -1, -1):
            if ep >= sp-1:
                # (analysis) print('adv: [' + str(sp) + ', ' + str(ep) + '] for ' + P[i] + ' with [' + str(sp) + ', ' + str(ep) + '] bringing (' + BWT[sp] + str(numBWT[sp]) + ', ' + BWT[ep] + str(numBWT[ep]) + ')')
                sp = C[alphabetToNum[P[i]]] + rank(P[i], BWT, sp)
                ep = C[alphabetToNum[P[i]]] + rank(P[i], BWT, ep + 1) - 1
                # in the Siren2014 paper, sp and ep would each be one higher

            else:
                return [-1, -1]

        # (analysis) print('adv res: [' + str(sp) + ', ' + str(ep) + ']')

        return [sp, ep]



        # the following source code is half-way between the normal BWT
        # and the advanced BWT implementation, to help understanding their
        # differences and how once translates into the other;
        # notice especially how find_rows_with_letter is here still used,
        # but becomes unnecessary in the advanced BWT version
        # 
        # P = lastline
        # lenP = len(P)
        #
        # [sp, ep] = [0, len(BWT)-1]
        # for i in range(lenP-1, -1, -1):
        #     [sp, ep] = find_rows_with_letter(P[i], BWT, sp, ep)
        #     if (ep > -1) and (ep >= sp-1):
        #         sp = C[alphabetToNum[P[i]]] + rank(P[i], BWT, sp)
        #         ep = C[alphabetToNum[P[i]]] + rank(P[i], BWT, ep)
        #     else:
        #         return [-1, -1]
        #
        # return [sp, ep]



    k = len(lastline)-1

    [startRow, endRow] = [0, len(BWT)-1]

    # (analysis) print(' ')
    # (analysis) print(' ')

    while (endRow > -1) and (k > -1):
        # (analysis) pr = 'old: [' + str(startRow) + ', ' + str(endRow) + '] for ' + lastline[k]
        [startRow, endRow] = find_rows_with_letter(lastline[k], BWT, startRow, endRow)
        if (endRow > -1):
            # (analysis) print(pr + ' with [' + str(startRow) + ', ' + str(endRow) + '] bringing (' + BWT[startRow] + str(numBWT[startRow]) + ', ' + BWT[endRow] + str(numBWT[endRow]) + ')')
            startRow = last_to_first(startRow, BWT, numBWT, firstCol, numfirstCol)
            endRow = last_to_first(endRow, BWT, numBWT, firstCol, numfirstCol)
            k -= 1

    # (analysis) print(' ')

    # (analysis) print('old res: [' + str(startRow) + ', ' + str(endRow) + ']')

    # suffix_array[startRow .. endRow+1] are equally perfect matches,
    # so we give back all of startRow up to endRow
    return [startRow, endRow]



# only used for BWT without pigeonhole algorithm
def findBestRow(lastline, BWT, numBWT, firstCol, numfirstCol, C, alphabetToNum, numToAlphabet, use_advanced_BWT):

    # (analysis) print(lastline)

    # actually, the results are equally perfect matches...
    # however, in the end we will have to choose one anyway, so let's just randomly
    # choose the last of the eligible rows for now
    # (we take [1], which is endRow, so the last row)
    ret = findBestRows(lastline, BWT, numBWT, firstCol, numfirstCol, C, alphabetToNum, numToAlphabet, use_advanced_BWT)

    # (analysis) print(ret)

    return ret[1]



# only used by advanced BWT (see Siren2014, but with or without pigeonhole algorithm)
def rank(c, BWT, i):
    return BWT.count(c, 0, i)



# --------------------------------------------------------------------------------------------
#     Main Function
# --------------------------------------------------------------------------------------------

def align_reads(referencepath, prepropath, sapath, freads, d, k, plen, useBWT, usepigeonhole):
    
    ret = []
    
    if (useBWT == 1):

        # advanced BWT as in Siren2014; otherwise more basic BWT is used
        use_advanced_BWT = True

        # load Burrows-Wheeler Transform

        fprepro = open(prepropath, 'r')
        BWT = fprepro.readline().strip()
        fprepro.close()

        # build numbering of the BWT (last column) for using the first-last property

        numBWT = []
        arrBWT = collections.defaultdict(lambda: 0)

        for i in range(0, len(BWT)):
            arrBWT[BWT[i]] += 1
            numBWT.append(arrBWT[BWT[i]])

        # construct first column

        firstCol = ''.join(sorted(BWT))

        # build numbering of the sorted BWT (first column) for using the first-last property

        numfirstCol = []
        arrfirstCol = collections.defaultdict(lambda: 0)

        for i in range(0, len(firstCol)):
            arrfirstCol[firstCol[i]] += 1
            numfirstCol.append(arrfirstCol[firstCol[i]])

        # We now have:
        #
        # smnpbnnaaaaa$a (BWT)
        # 11111231234516 (numBWT)
        #
        # $aaaaaabmnnnps (firstCol)
        # 11234561112311 (numfirstCol)


        numToAlphabet = []
        alphabetToNum = {}
        C = [0]


        if use_advanced_BWT:
            # create the C-array
            # where C[0, s°+1] be such that C[c] := # of characters from {$, 1, 2, ..., c-1} in the BWT,
            # with C[0] = C[$] = 0 and C[s°+1] = n = |BWT|
            # we here build it by simply going through numfirstCol / firstCol and assigning
            # stuff to C - this is a very simple way of building it, albeit not the best way ^^
            # (also, we build the alphabet that is used right here as well)
            lastchar = ''
            j = 0
            for i in range(0, len(firstCol)):
                if firstCol[i] != lastchar:
                    lastchar = firstCol[i]
                    alphabetToNum[lastchar] = len(numToAlphabet)
                    numToAlphabet.append(lastchar)
                    C.append(C[j])
                    j = j+1
                C[j] += 1

            # we now have:
            #                  0  1  2  3  4  5  6
            # numToAlphabet = [$, a, b, m, n, p, s]
            # alphabetToNum = [$: 0, a: 1, b: 2, m: 3, n: 4, p: 5, s: 6]
            #
            # C[0] = C[$] = 0
            # C[1] = C[a] = 1
            # C[2] = C[b] = 7
            # C[3] = C[m] = 8
            # C[4] = C[n] = 9
            # C[5] = C[p] = 12
            # C[6] = C[s] = 13
            # C[7]        = 14



        # load suffix array

        suffix_array = []

        fprepro = open(sapath, 'r')
        lastline = fprepro.readline().strip()

        while (lastline != ""):
            suffix_array.append(int(lastline))
            lastline = fprepro.readline().strip()

        fprepro.close()


        lastline = freads.readline().strip()

        if usepigeonhole:

            # use pigeonhole approach to find slightly mismatched locations

            reference = loadTextFromFile(referencepath)

            while (lastline != ""):

                pigeon = pigeonholeBWT(lastline, reference, d, k, suffix_array, BWT, numBWT, firstCol, numfirstCol, plen, C, alphabetToNum, numToAlphabet, use_advanced_BWT)
                lenP = len(pigeon)-1

                if lenP >= 0:
                    pigeon.sort()

                    # (analysis) print("\nsorted: " + str(pigeon));

                    # we take the position with the highest score - that is,
                    # the last after sorting incrementally - to get the position
                    # with the lowest mismatch count
                    ret.append(str(pigeon[lenP][1]) + pigeon[lenP][2])
                else:
                    ret.append("")
    
                lastline = freads.readline().strip()

        else:
            
            # do not use pigeonhole approach and only find exact locations
            while (lastline != ""):

                bestRow = findBestRow(lastline, BWT, numBWT, firstCol, numfirstCol, C, alphabetToNum, numToAlphabet, use_advanced_BWT)

                if (bestRow > -1):
                    ret.append("[" + str(suffix_array[bestRow]) + ", 0]")
                else:
                    ret.append("")

                lastline = freads.readline().strip()
    
    else:

        # load pre-processed suffix array with texts

        suffix_array = collections.defaultdict(lambda: '')

        fprepro = open(prepropath, 'r')
        lastline = fprepro.readline().strip()

        while (lastline != ""):
            lastline = lastline.split(" -> ")
            suffix_array[lastline[0]] += lastline[1]
            lastline = fprepro.readline().strip()

        fprepro.close()


        reference = loadTextFromFile(referencepath)
        
        lastline = freads.readline().strip()

        while (lastline != ""):
            pigeon = pigeonhole(lastline, reference, d, k, suffix_array, plen)
            lenP = len(pigeon)-1
            if lenP >= 0:
                pigeon.sort()
                # we take the position with the highest score - that is,
                # the last after sorting incrementally - to get the position
                # with the lowest mismatch count
                ret.append(str(pigeon[lenP][1]) + pigeon[lenP][2])
            else:
                ret.append("")
            lastline = freads.readline().strip()
    
    return ret


def strarraytostring(arr):
    
    return "\n".join(arr)



res = strarraytostring(align_reads(referencepath, prepropath, sapath, freads, d, k, plen, useBWT, usepigeonhole))

freads.close()

fo = open('out.txt', 'w')

fo.write(res)

fo.close()
