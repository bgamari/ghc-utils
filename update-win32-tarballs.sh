#!/bin/bash -e

gpg --recv-key  5F92EFC1A47D45A1

# Set mingw_base_url in mk/get-win32-tarballs.sh to the upstream repo.
mk/get-win32-tarballs.sh fetch mirror
for f in $(find ghc-tarballs/mingw-w64 -iname '*.sig'); do
    echo "Verifying $f"
    gpg --verify $f
done
md5sum `find ghc-tarballs -type f -a -not -iname '*.sig'` >| mk/win32-tarballs.md5sum

chmod -R ugo+rX ghc-tarballs
rsync -av ghc-tarballs/mingw-w64/* ghc-downloads.haskell.org:public_html/mingw
for f in $(find ghc-tarballs/mingw-w64); do
    curl -XPURGE http://downloads.haskell.org/~ghc/mingw/$f
done
