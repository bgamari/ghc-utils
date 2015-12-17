#!/bin/bash

failing_tests() {
    file=$1
    if [ -z "$file" ]; then file="testsuite_summary.txt"; fi
    TEST=$(sed -n 's/^TEST="\(.*\)"$/\1/p' $file)
    echo "TEST=$TEST"
}

