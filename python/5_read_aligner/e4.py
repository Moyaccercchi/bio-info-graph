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
# by Moyaccercchi

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


# check if a Pattern is a match with at most d mismatches at a given position in the text
def isamatch(Pattern, Text, d, position):

    # eliminate strings that start before the text
    if position < 0:
        return 0

    # eliminate strings that don't fit into the text
    if position+len(Pattern) > len(Text):
        return 0

    # eliminate strings that have too many mismatches
    for i in range(0, len(Pattern)):
        if (Text[position+i] != Pattern[i]):
            d -= 1
            if d < 0:
                return 0
    return 1


# call the pigeonhole algorithm
def pigeonhole(Pattern, Text, d, suffix_array, plen):
    
    j = 0
    lenP = len(Pattern)
    parts = d + 1
    possible_ret = []
    
    for i in range(1, parts+1):
        newj = j + plen
        if newj > lenP:
            newj = lenP
            j = newj - plen
        sufarr = suffix_array[Pattern[j:newj]]
        if len(sufarr) > 0:
            lastfind = sufarr.find(',')
            while lastfind > -1:
                possible_ret.append(int(sufarr[0:sufarr.find(',')-1]) - j)
                sufarr = sufarr[sufarr.find(',')+1:]
            possible_ret.append(int(sufarr) - j)
        j = newj
    
    # convert to set to eliminate double entries, back to list, then sort
    possible_ret = sorted(list(set(possible_ret)))
    
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
        if isamatch(Pattern, Text, d+3, possible_ret[i]):
            ret.append(possible_ret[i])
    
    return ret


def align_reads(reference, suffix_array, freads, d, plen):
    
    ret = []
    
    lastline = freads.readline().strip()
    
    while (lastline != ""):
        pigeon = pigeonhole(lastline, reference, d, suffix_array, plen)
        if len(pigeon) > 0:
            # TODO :: do not just use the first one, but actually choose the best one
            ret.append(str(pigeon[0]))
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
