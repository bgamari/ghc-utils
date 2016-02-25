#!/usr/bin/python

import sys
import os
import re

f = sys.argv[1]
i = 0

os.mkdir('%s.split' % f)
output = open('%s.split/out-00' % f, 'w')
for ln in open(f):
    if ln.startswith('======='):
        name = ln.strip('= ')
        name = re.match(r'[a-z0-9 ]*', name.lower()).group(0)
        name = name.strip().replace(' ', '-')
        i += 1
        print i, name
        output = open('%s.split/out-%02d-%s' % (f, i, name), 'w')

    output.write(ln)

