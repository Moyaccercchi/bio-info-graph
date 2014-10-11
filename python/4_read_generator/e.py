#!/usr/local/bin/python
# The previous line (which must be the first one to work) makes the script self-executing,
# assuming that the system has the Python interpreter at path /usr/local/bin/python.

# This wants to be run in Python 2 or 3.


# Read Generator
# Given: A string text
#        A length k for each read
#        A total amount amt of reads that are generated
#        A mistake probability misprob for generating the reads
#        A string alphabet that contains every character that can occur
# Return: A list of generated reads
#         A second list of positions of these generated reads
# by Moyaccercchi

import random

random.seed()

fo = open('in.txt', 'r')

text = fo.readline().strip()
k = int(fo.readline().strip())
amt = int(fo.readline().strip())
misprob = int(fo.readline().strip())
alphabet = fo.readline().strip()

fo.close()


def generate_reads(text, k, amt, misprob, alphabet):
    
    ret = []
    retpos = []
    
    maxoffset = len(text) - k
    alen = len(alphabet)
    
    for i in range(0, amt):
        offset = random.randint(0, maxoffset)
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


res,respos = generate_reads(text, k, amt, misprob, alphabet)

fo = open('outpos.txt', 'w')

fo.write(respos)

fo.close()

fo = open('out.txt', 'w')

fo.write(res)

fo.close()
