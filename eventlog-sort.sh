#!/bin/sh
set -e
ghc-events show $1 | awk 'BEGIN{t0=0} {t=$1; printf "%13f  %13f   %s\n", ((t-t0)/1000/1000), (t/1000/1000), $0; t0=t; }'
