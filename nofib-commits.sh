#!/bin/bash -e

commits=$(git rev-parse --reverse $@)
if [[ -z "$nruns" ]]; then
    nruns=1
fi

mkdir -p logs
echo "Testing $(echo $commits | wc -l) commits with $nruns runs each"
i=0
root=$(pwd)
for commit in $commits; do
    echo "checking out $commit..."
    git checkout $commit
    git submodule update --init

    echo "building $commit..."
    make -j9 stage=2 >logs/build-$commit.log 2>&1
    if [ ! -e inplace/bin/ghc-stage2 ]; then
        echo "Uh oh, no compiler found!"
        exit 1
    fi

    echo "running nofib on $commit..."
    cd nofib
    cabal install --with-ghc=$root/inplace/bin/ghc-stage2 --disable-library-profiling --force-reinstalls regex-compat html
    make clean
    make -j8 boot >logs/nofib-boot-$commit.log 2>&1
    for run in $(seq $nruns); do
        echo "  run $i"
        (
          git -C .. show --pretty=oneline
          make
        ) >logs/nofib-$(printf "%03d" $i)-$commit-run$(printf "%03d" $run).log 2>&1
    done
    let i=i+1
    cd ..
done

