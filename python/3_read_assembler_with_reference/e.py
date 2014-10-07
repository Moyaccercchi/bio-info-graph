#!/usr/local/bin/python
# The previous line (which must be the first one to work) makes the script self-executing,
# assuming that the system has the Python interpreter at path /usr/local/bin/python.

# This wants to be run in Python 2 or 3.


# Suffix-Thingy
# Given: Big string dna, several short reads.
# Return: The positions of the reads.

import time, collections, random
from collections import defaultdict
from random import randrange

fo = open('in.txt', 'r')

k,t,N = fo.readline().strip().split(" ")
k = int(k)
t = int(t)
N = int(N)
dna = []
lastline = fo.readline().strip()
while (lastline != ""):
  dna.append(lastline)
  lastline = fo.readline().strip()

fo.close()


def getprofileprob(Profile, kmer):
    
    ret = 1
    letterToInt = {"A":0,"C":1,"G":2,"T":3}
    
    for i in range(0, len(kmer)):
        ret *= Profile[letterToInt[kmer[i]]][i]
    
    return ret


def profilemostprob(Text, k, Profile):
    
    allprobs = []
    totalprobs = 0
    
    for pos in range(0, 1 + len(Text) - k):
        allprobs.append(getprofileprob(Profile, Text[pos:pos+k]))
        totalprobs += allprobs[pos]
    
    lcv = random.uniform(0, totalprobs);
    totalprobs = 0
    
    for pos in range(0, 1 + len(Text) - k):
        totalprobs += allprobs[pos]
        if lcv < totalprobs:
            return pos
    
    return 0


def score_advanced(positions, dna, k, Profile):

    # now... score that!
    
    letterToInt = {"A":0,"C":1,"G":2,"T":3}
    totalprobs = len(dna)
    for d in range(0, len(dna)):
        probs = 1
        for i in range(0, k):
            probs *= Profile[letterToInt[dna[d][i+positions[d]]]][i]
        totalprobs -= probs
    
    return totalprobs


def score(positions, dna, k, Profile):

    # generate consensus string
    
    consensus = ""
    intToLetter = ["A", "C", "G", "T"]
    
    for i in range(0, k):
        curchar = 0
        curcount = 0
        for j in range(0, 4):
            if Profile[j][i] > curcount:
                curcount = Profile[j][i]
                curchar = j
        consensus += intToLetter[curchar]
    
    # now... score that!
    
    differences = 0
    for d in range(0, len(dna)):
        for i in range(0, k):
            if consensus[i] != dna[d][i+positions[d]]:
                differences += 1
    
    return differences


def randGibbssearch(k, t, N, dna):
    
    positions = []
    oldposstr = ""
    newposstr = ""
    consensus = ""
    bestscore = 100000
    
    for i in range(0, len(dna)):
        positions.append(randrange(0, 1 + len(dna[i]) - k))
    newposstr = " ".join(str(x) for x in positions)
    
    bestpositions = []
    for i in range(0, len(positions)):
        bestpositions.append(positions[i])
    
    # build Profile matrix with pseudocounts
    Profile = []
    for i in range(0, 4):
        Profile.append([1] * k)
    
    letterToInt = {"A":0,"C":1,"G":2,"T":3}
    
    for d in range(0, len(dna)):
        for i in range(0, k):
            Profile[letterToInt[dna[d][i+positions[d]]]][i] += 1

    # normalize Profile
    
    for d in range(0, k):
        colsum = 0
        for j in range(0, 4):
            colsum += Profile[j][d]
        for j in range(0, 4):
            Profile[j][d] = Profile[j][d] / colsum

    # calculate scoring differences
    bestscore = score(bestpositions, dna, k, Profile)
    bestscore_advanced = score_advanced(bestpositions, dna, k, Profile)
    
    for somevariable in range(0, N):
    
        # calculate most probable motif for this profile
        
        oldposstr = newposstr
        newposstr = " ".join(str(x) for x in positions)
        
        d = randrange(0, len(dna))
        positions[d] = profilemostprob(dna[d], k, Profile)
        
        # build Profile matrix with pseudocounts
        Profile = []
        for i in range(0, 4):
            Profile.append([1] * k)
        
        letterToInt = {"A":0,"C":1,"G":2,"T":3}
        
        for d in range(0, len(dna)):
            for i in range(0, k):
                Profile[letterToInt[dna[d][i+positions[d]]]][i] += 1
    
        # normalize Profile
        
        for d in range(0, k):
            colsum = 0
            for j in range(0, 4):
                colsum += Profile[j][d]
            for j in range(0, 4):
                Profile[j][d] = Profile[j][d] / colsum
    
        curscore = score(positions, dna, k, Profile)
        curscore_advanced = score_advanced(positions, dna, k, Profile)
        
        if (curscore < bestscore) or ((curscore == bestscore) and (curscore_advanced < bestscore_advanced)):
            bestpositions = []
            for i in range(0, len(positions)):
                bestpositions.append(positions[i])
            bestscore = curscore
            bestscore_advanced = curscore_advanced
        else:
            positions = []
            for i in range(0, len(bestpositions)):
                positions.append(bestpositions[i])
    
    return [" ".join(str(x) for x in bestpositions), bestscore, bestscore_advanced]


def dosearch(k, t, N, dna, times):
    
    results = defaultdict(lambda: 0)
    
    for i in range(0, times):
        res = randGibbssearch(k, t, N, dna)
        results[res[0]] = [res[1], res[2]]
    
    maxkey = ""
    maxval = [10000000000, 10000000000]
    
    for key, value in results.items():
        if (value[0] < maxval[0]) or ((value[0] == maxval[0]) and (value[1] < maxval[1])):
            maxval = value
            maxkey = key
    
    print("winner:")
    print(maxkey)
    print("value of winner:")
    print(maxval[0])
    print("advanced value of winner:")
    print(maxval[1])
    
    ret = list(int(i) for i in maxkey.split(" "))

    for i in range(0, len(ret)):
        ret[i] = dna[i][ret[i]:ret[i]+k]
    
    return ret


def strarraytostring(arr):
    
    return "\n".join(arr)


random.seed()

timebefore = time.time()
res = 'best motifs:\n' + strarraytostring(dosearch(k, t, N, dna, 20)) + '\n'
res += 'time for 20 x GibbsSampling: ' + str(time.time() - timebefore) + ' s'

fo = open('out.txt', 'w')

fo.write(res)

fo.close()

print(res)
