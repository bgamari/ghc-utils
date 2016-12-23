src=Hi
ghc1=/opt/exp/ghc/ghc-compare-1/inplace/bin/ghc-stage2
ghc2=/opt/exp/ghc/ghc/inplace/bin/ghc-stage2

HCOPTS="-rtsopts -ddump-to-file -fforce-recomp -dsuppress-coercions -dsuppress-idinfo -ddump-stg -ddump-simpl -dsuppress-uniques -dverbose-core2core -ddump-inlinings"
EXTRA_HCOPTS="-ticky -ticky-allocd -funfolding-use-threshold=200 -O2"
RUN_OPTS="+RTS -s -RTS"

compile() {
    set -x
    (
        mkdir -p 1
        cd 1
        cp ../$src.hs .
        echo "$ghc1 $HCOPTS $EXTRA_HCOPTS $src" > $src.log
        $ghc1 $HCOPTS $EXTRA_HCOPTS $src 2>&1 >> $src.log

        rm -Rf $src.verbose-core2core.split
        split-core2core.py $src.verbose-core2core
    )
    (
        mkdir -p 2
        cd 2
        cp ../$src.hs .
        echo "$ghc2 $HCOPTS $EXTRA_HCOPTS $src" > $src.log
        $ghc2 $HCOPTS $EXTRA_HCOPTS $src 2>&1 >> $src.log

        rm -Rf $src.verbose-core2core.split
        split-core2core.py $src.verbose-core2core
    )
    set +x
}

compare() {
    file=$1
    meld 1/$file 2/$file
}

function _comp_compare() {
    pushd 1 >/dev/null
    COMPREPLY=( $(compgen -o filenames -A file $2))
    popd >/dev/null
}
complete -F _comp_compare compare

run() {
    if [ ! -f run.stdin ]; then touch run.stdin; fi
    (
        cd 1
        ./$src $RUN_OPTS < ../run.stdin >| $src.run 2>&1
    )
    (
        cd 2
        ./$src $RUN_OPTS < ../run.stdin >| $src.run 2>&1
    )
}

echo "GHC 1: $ghc1"
echo "GHC 2: $ghc2"
echo "Module: $src"
echo "GHC options: $HCOPTS $EXTRA_HCOPTS"
echo "Run options: $RUN_OPTS"
