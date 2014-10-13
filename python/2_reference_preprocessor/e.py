#!/usr/local/bin/python
# The previous line (which must be the first one to work) makes the script self-executing,
# assuming that the system has the Python interpreter at path /usr/local/bin/python.

# This wants to be run in Python 3.


# Reference Pre-Processor
# Given: A string reference
#        An integer horizon, which tells us how far to look ahead if we do look ahead
# Return: A suffix array
# by Moyaccercchi

import collections

fo = open('in.txt', 'r')

reference = fo.readline().strip()
horizon = int(fo.readline().strip())

fo.close()


def generate_suffix_array(reference, horizon):
    
    ret = collections.defaultdict(lambda: '')
    
    for i in range(0, len(reference)):
        ret[reference[i:i+horizon]] += str(i) + ","
    
    return ret


def dicttoadjacency(ourdict):
    
    ret = []
    
    for fromnode, tonode in ourdict.items():
        ret.append(fromnode + ' -> ' + tonode[:-1])
    
    return '\n'.join(sorted(ret))


res = dicttoadjacency(generate_suffix_array(reference, horizon))

fo = open('out.txt', 'w')

fo.write(res)

fo.close()
