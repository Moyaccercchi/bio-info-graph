#!/usr/local/bin/python
# The previous line (which must be the first one to work) makes the script self-executing,
# assuming that the system has the Python interpreter at path /usr/local/bin/python.

# This wants to be run in Python 2 or 3.


# Read Generator
# Given: A string text
#        A length k for each read
#        A total amount amt of reads that are generated
#        A mistake probability misprob for generating the reads
# Return: A list of generated reads

import random

random.seed()

fo = open('in.txt', 'r')

text = fo.readline().strip()
k = int(fo.readline().strip())
amt = int(fo.readline().strip())
misprob = int(fo.readline().strip())

fo.close()


def generate_reads(text, k, amt, misprob):
    
    ret = []
    
    maxoffset = len(text) - k
    
    for i in range(0, amt):
        offset = random.randint(0, maxoffset)
        ret.append(text[offset:k+offset])
    
    return ret


def strarraytostring(arr):
    
    return "\n".join(arr)


res = strarraytostring(generate_reads(text, k, amt, misprob))

fo = open('out.txt', 'w')

fo.write(res)

fo.close()
