#!/bin/bash -e

commits=$(git rev-parse $@)
if [[ -z "$nruns" ]]; then
    nruns=1
fi

do_commit() {
    seq=$(printf "%03d" $i)
    echo "checking out $commit..."
    git checkout $commit
    git submodule update --init

    echo "building $commit..."
    ( make -j9 stage=2 || (make clean; make -j9) ) >$logs/build-$seq-$commit.log 2>&1
    if [ ! -e inplace/bin/ghc-stage2 ]; then
        echo "Uh oh, no compiler found!"
        exit 1
    fi

    echo "running nofib on $commit..."
    cd nofib
    cabal install --with-ghc=$root/inplace/bin/ghc-stage2 --disable-library-profiling --force-reinstalls regex-compat html >log 2>&1
    make clean >log 2>&1
    make -j8 boot >$logs/nofib-boot-$seq-$commit.log 2>&1
    for run in $(seq $nruns); do
        echo "  run $i"
        (
          git -C .. show --pretty=oneline
          make
        ) >$logs/nofib-$seq-$commit-run$(printf "%03d" $run).log 2>&1
    done
    let i=i+1
    cd ..
}

cabal install --disable-library-profiling --force-reinstalls regex-compat html >log 2>&1
logs=`pwd`/logs
mkdir -p $logs
echo "Testing $(echo $commits | wc -l) commits with $nruns runs each"
i=0
root=$(pwd)
for commit in $commits; do
    do_commit $commit
done

