#!/usr/bin/python

import logging
from glob import glob
import os
import os.path
import argparse
import pystache

parser = argparse.ArgumentParser()
parser.add_argument('version', help='The GHC version being released (e.g. 7.10.2)')
parser.add_argument('tarball_dir', help='The path to a directory containing the release tarballs')
parser.add_argument('--download-url', metavar='URL',
                    help='The URL where the tarballs will be available. By default http://downloads.haskell.org/~ghc/$version')
args = parser.parse_args()

version = args.version
tarball_dir = args.tarball_dir
download_url = args.download_url
if download_url is None:
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
        # Here we look at all patch levels of the desired version
        pattern = 'ghc-{ver}*-{suffix}.{ext}'.format(ver=version, suffix=suffix, ext=ext)
        paths = glob(os.path.join(tarball_dir, pattern))

        if len(paths) == 0:
            logging.error("Couldn't find %s" % path)
            size = 'unknown size'
            fname = 'unknown'
        else:
            # Here we take just the latest one
            path = sorted(paths)[-1]
            fname = os.path.basename(path)
            size = os.stat(path).st_size
            size = '%d MB' % (size / 1024 / 1024)

        used_files.union(paths)
        logging.debug('Saw %s' % paths)

        accum += '<li><a href="{root_url}/{fname}">{fname}</a> ({size})</li>\n'.format(
            root_url=download_url, fname=fname, size=size)

    accum += '</ul>\n'
    return accum

context = {
    'tarballs': handle_tarballs,
    'download_url': download_url,
    'ver': version,
}
template = open('download.shtml.must').read().decode('utf8')
with open('download_ghc_%s.shtml' % (version.replace('.', '_')), 'w') as f:
    f.write(pystache.render(template, context).encode('utf-8'))

for fname in known_files - used_files:
    logging.warning("Didn't see reference to %s" % fname)
