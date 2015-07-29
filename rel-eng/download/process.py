#!/usr/bin/python

import logging
from glob import glob
import os
import os.path
import pystache

version = '7.10.2'
tarball_dir = '/home/ben/tmp'
download_url = 'http://downloads.haskell.org/~ghc/{ver}'.format(ver=version)

logging.basicConfig(level=logging.DEBUG)

extensions = ['tar.bz2', 'tar.xz']
known_files = set(os.path.basename(path)
                  for ext in extensions
                  for path in glob(os.path.join(tarball_dir, '*.'+ext)))
used_files = set()

def handle_tarballs(text):
    suffix = text
    accum = '<ul>\n'
    for ext in extensions:
        fname = 'ghc-{ver}-{suffix}.{ext}'.format(ver=version, suffix=suffix, ext=ext)
        path = os.path.join(tarball_dir, fname)
        if not os.path.isfile(path):
            logging.error("Couldn't find %s" % path)
            size = 'unknown size'
        else:
            size = os.stat(path).st_size
            size = '%d MB' % (size / 1024 / 1024)
            used_files.add(fname)
            logging.debug('Saw %s' % fname)

        accum += '<li><a href="{root_url}/{fname}">{fname}</a> ({size})</li>\n'.format(
            root_url=download_url, fname=fname, size=size)

    accum += '</ul>\n'
    return accum

context = {
    'tarballs': handle_tarballs,
    'ver': version,
}
template = open('download.shtml.must').read()
print pystache.render(template, context)

for fname in known_files - used_files:
    logging.warning("Didn't see reference to %s" % fname)
