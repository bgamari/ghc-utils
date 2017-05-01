#!/usr/bin/python

import sys
import os
import re

f = sys.argv[1]
base = os.path.splitext(f)[0]
i = 0

os.mkdir('%s.split' % f)
output = open('%s.split/%s-00' % (f, base), 'w')
for ln in open(f):
    if ln.startswith('======='):
        name = ln.strip('= ')
        name = re.match(r'[a-z0-9 ]*', name.lower()).group(0)
        name = name.strip().replace(' ', '-')
        i += 1
        print i, name
        output = open('%s.split/%s.%02d-%s' % (f, base, i, name), 'w')

    output.write(ln)

