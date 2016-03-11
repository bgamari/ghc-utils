#!/bin/bash -e

hasktags -x -o TAGS.hs -e compiler ghc libraries/{base,ghc-boot,ghc-prim,ghci,hoopl} iserv
cd rts
etags $(find -iname '*.c')
cd ..
etags --include rts/TAGS --include TAGS.hs
