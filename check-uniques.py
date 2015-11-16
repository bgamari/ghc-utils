#!/usr/bin/env python

from __future__ import print_function
import sys
import re
import glob
from collections import defaultdict

# keyed on unique type, values are lists of (unique, name) pairs
def find_uniques(source_files):
    uniques = defaultdict(lambda: defaultdict(lambda: set()))
    unique_re = re.compile(r"([\w\d]+)\s*=\s*mk([\w\d']+)Unique\s+(\d+)")
    for f in source_files:
        ms = unique_re.findall(open(f).read())
        for m in ms:
            name = m[0]
            _type = m[1]
            n = int(m[2])
            uniques[_type][n].add(name)

    return uniques

def print_all(uniques):
    for _type, uniqs in uniques.items():
        print('{_type} uniques'.format(**locals()))
        for n,names in uniqs.items():
            all_names = ', '.join(names)
            print('  {n} = {all_names}'.format(**locals()))

def check_conflicts(uniques):
    fail = False
    for _type, uniqs in uniques.items():
        for n,names in uniqs.items():
            all_names = ', '.join(names)
            if len(names) > 1:
                print('{_type} unique {n} conflict: {all_names}'.format(**locals()))
                fail = True

    return fail

uniques = find_uniques(glob.glob('compiler/prelude/*.hs'))
print_all(uniques)
if check_conflicts(uniques):
    sys.exit(1)
