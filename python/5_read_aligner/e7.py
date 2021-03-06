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
# by Moyaccercchi, 24th Jan 2015
# 
# version 7:
# improved indel handling for backward matching
# 
# the string "# (analysis) " can be globally replaced with an empty string to obtain debug output

import collections

fo = open('in.txt', 'r')

reference = fo.readline().strip()
prepropath = fo.readline().strip()
readpath = fo.readline().strip()
d = int(fo.readline().strip())
plen = int(fo.readline().strip())

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
    
    lenP = len(Pattern)
    
    for i in range(0, lenP):
        if (Text[i] != Pattern[i]):
            d -= 1
            if d < abortat:
                return [d, ""]
            
            # edits are most likely, so we try these first
            b,binf = matchwithindelsfwd(Text[i+1:lenP], Pattern[i+1:lenP], d, i+j+1, 0)
            
            # indels are less likely, so we try them afterwards
            # ideally, we have at this stage already found a pretty solid
            # edit-solution and can simply skip when abortat is underrun
            a,ainf = matchwithindelsfwd(Text[i:lenP-1], Pattern[i+1:lenP], d, i+j+1, max(0, b))
            c,cinf = matchwithindelsfwd(Text[i+2:lenP], Pattern[i+1:lenP-1], d, i+j+1, max(0, a, b))
            
            ainf = "ins at " + str(i+j) + ", " + ainf
            cinf = "del at " + str(i+j) + ", " + cinf
            
            # (analysis) print("fwd  a: " + str(a) + " b: " + str(b) + " c: " + str(c) + " i: " + str(i))
            
            if c > b:
                if c > a:
                    maxd = c
                    maxinf = cinf
                else:
                    maxd = a
                    maxinf = ainf
            else:
                if b > a:
                    maxd = b
                    maxinf = binf
                else:
                    maxd = a
                    maxinf = ainf
            
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
            
            # (analysis) print("bwd  a: " + str(a) + " b: " + str(b) + " c: " + str(c) + " i: " + str(i))
            
            if c > b:
                if c > a:
                    maxd = c
                    maxinf = cinf
                else:
                    maxd = a
                    maxinf = ainf
            else:
                if b > a:
                    maxd = b
                    maxinf = binf
                else:
                    maxd = a
                    maxinf = ainf
            
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


# call the pigeonhole algorithm
# Pattern .. the read that we currently analyse
# Text .. the reference string
# d .. maximum amount of mismatches
# suffix_array .. the pre-processed reference as suffix array
# plen .. length for parts of the pigeonhole algorithm, that is, roughly len(Pattern) / (d+1)
def pigeonhole(Pattern, Text, d, suffix_array, plen):
    
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
        # we actually allow for d+3 mismatches for improved performance;
        # that is, as mismatches often appear next to each other, we might
        # be lucky and several mismatches fall into the same interval such
        # that we don't need to go up with the main d, but only the sub-d
        # here - increasing this d here by 3 leads to significantly improved
        # results (from 73% correct assembly up to 96% correct assembly)
        # while still being a lot less expensive than increasing the main d
        # even by just 1
        ismatch,scoreofmatch,infostr = isamatch(Pattern, Text, d+3, possible_ret[i], plen)
        if ismatch:
            if len(infostr) > 0:
                infostr = " [" + infostr + "]"
            ret.append([scoreofmatch, possible_ret[i], infostr])
    
    return ret


def align_reads(reference, suffix_array, freads, d, plen):
    
    ret = []
    
    lastline = freads.readline().strip()
    
    while (lastline != ""):
        pigeon = pigeonhole(lastline, reference, d, suffix_array, plen)
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


res = strarraytostring(align_reads(reference, suffix_array, freads, d, plen))

freads.close()

fo = open('out.txt', 'w')

fo.write(res)

fo.close()
