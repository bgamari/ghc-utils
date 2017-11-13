#!/usr/bin/env python3

import sys
import os
import re

def split_file(f):
    base = os.path.basename(f)[0]
    i = 0

    print(f)
    os.mkdir('%s.split' % f)
    output = open('%s.split/%s-00' % (f, base), 'w')
    for ln in open(f):
        if ln.startswith('======='):
            name = ln.strip('= ')
            name = re.match(r'[a-z0-9 ]*', name.lower()).group(0)
            name = name.strip().replace(' ', '-')
            i += 1
            print(i, name)
            output = open('%s.split/%s.%02d-%s' % (f, base, i, name), 'w')

        output.write(ln)

if __name__ == '__main__':
    for f in sys.argv[1:]:
        split_file(f)
