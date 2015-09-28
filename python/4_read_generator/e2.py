#!/usr/local/bin/python
# The previous line (which must be the first one to work) makes the script self-executing,
# assuming that the system has the Python interpreter at path /usr/local/bin/python.

# This wants to be run in Python 3.


# Read Generator
# Given: A string text
#        A length k for each read
#        A total amount amt of reads that are generated
#        A mistake probability misprob for generating the reads
#        A string alphabet that contains every character that can occur
# Return: A list of generated reads
#         A second list of positions of these generated reads
#
# by Moyaccercchi, 8th of Feb 2015
#
# version 2:
# added FASTA support

import random

random.seed()

fo = open('in.txt', 'r')

individualpath = fo.readline().strip()
k = int(fo.readline().strip())
amt = int(fo.readline().strip())
misprob = int(fo.readline().strip())
alphabet = fo.readline().strip()

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


def generate_reads(individualpath, k, amt, misprob, alphabet):
    
    ret = []
    retpos = []
    
    text = loadTextFromFile(individualpath)

    maxoffset = len(text) - k
    alen = len(alphabet)
    # We want to give a fair chance that reads start at 0 or end at the very end;
    # to do so, we add a read length before the beginning and a read length after the end
    randlow = - k
    randhigh = maxoffset + k
    
    for i in range(0, amt):
        offset = random.randint(randlow, randhigh)
        if offset < 0:
            offset = 0
        if offset > maxoffset:
            offset = maxoffset
        finalread = ''
        for j in range(0, k):
            if random.randint(0, 100) < misprob:
                finalread += alphabet[random.randint(0, alen-1)]
            else:
                finalread += text[offset + j]
        ret.append(finalread)
        retpos.append(str(offset))
    
    res = "\n".join(ret)
    respos = "\n".join(retpos)
    
    return (res,respos)


res,respos = generate_reads(individualpath, k, amt, misprob, alphabet)

fo = open('outpos.txt', 'w')

fo.write(respos)

fo.close()

fo = open('out.txt', 'w')

fo.write(res)

fo.close()
