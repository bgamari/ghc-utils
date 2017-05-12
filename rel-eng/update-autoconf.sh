#!/bin/bash -e

tmp=$(tempfile)

curl -s 'https://git.savannah.gnu.org/gitweb/?p=config.git;a=blob_plain;f=config.guess;hb=HEAD' > $tmp
for i in $(git ls-files | grep config.guess); do
    echo $i
    cp $tmp $i
done

curl -s 'https://git.savannah.gnu.org/gitweb/?p=config.git;a=blob_plain;f=config.sub;hb=HEAD' > $tmp
for i in $(git ls-files | grep config.sub); do
    echo $i
    cp $tmp $i
done

rm $tmp
