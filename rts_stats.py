#!/usr/bin/env python3

# Utility for comparing output from GHC's +RTS -t --machine-readable output.
"""
Given a set of runtime metrics (by default bytes_allocated and
total_cpu_seconds, overridden by --stat) and a set of logs from GHC's "-t
--machine-readable" flag, this will output the absolute metrics from the first
(reference) log followed by the relative changes of the other logs.

Example:

      # run the program like this
    $ ghc -rtsopts Fib.hs
    $ ./Fib 10 +RTS -tfib10.log --machine-readable
    $ ./Fib 12 +RTS -tfib12.log --machine-readable

      # compares allocations and CPU time by default
    $ python compare-stats.py fib10.log fib12.log
    fib1.log    allocated_bytes:    336496.0
    fib1.log    total_cpu_seconds:    0.000601
    fib2.log    allocated_bytes:   84.82002757833675%
    fib2.log    total_cpu_seconds:   51.41430948419302%

      # but you can also compare arbitrary metrics...
    $ python compare-stats.py -scopied_bytes -smax_bytes_used fib10.log fib12.log
    atom1.log    copied_bytes:    3480.0
    atom1.log    max_bytes_used:    44576.0
    atom2.log    copied_bytes:   0.0%
    atom2.log    max_bytes_used:   0.0%
"""

import re
import sys
import ast
import json
import typing

def read_rts_stats(f: typing.TextIO) -> dict:
    f.readline()
    raw = ast.literal_eval(re.sub('^ +', '', f.read()))
    parsed = { key: float(value) for key, value in raw }
    return parsed

def main() -> None:
    import argparse
    parser = argparse.ArgumentParser(
        description="Utility for comparing output from GHC's +RTS -t --machine-readable output.",
        epilog=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    subparsers = parser.add_subparsers()
    subparser = subparsers.add_parser('compare',
                                      help="Compare output from GHC's +RTS -t --machine-readable output")
    subparser.add_argument('-s', '--stat', type=str, action='append', default=[], metavar='METRIC',
                           help='Specify the runtime metrics to compare.')
    subparser.add_argument('-o', '--output', type=argparse.FileType('w'), default=sys.stdout,
                           help='Where to write the output (stdout by default)')
    subparser.add_argument('reference', type=argparse.FileType('r'), metavar="REF",
                           help='The reference log')
    subparser.add_argument('inputs', type=argparse.FileType('r'), nargs='+', metavar="FILE",
                           help='The logs to compare')
    subparser.set_defaults(func=compare)

    subparser = subparsers.add_parser('to-json')
    subparser.add_argument('-o', '--output', type=argparse.FileType('w'), default=sys.stdout,
                           help='Where to write the output (stdout by default)')
    subparser.add_argument('inputs', type=argparse.FileType('r'), nargs='+', metavar="FILE",
                           help='The logs to compare')
    subparser.set_defaults(func=to_json)

    args = parser.parse_args()
    args.func(args)

def to_json(args):
    files = { f.name: read_rts_stats(f) for f in args.inputs }
    json.dump(files, args.output)

def compare(args):
    ref = read_rts_stats(args.reference)
    for f in args.inputs:
        parsed = read_rts_stats(f)
        for stat in stats:
            x = ref[stat]
            y = parsed[stat]
            delta = (y - x) / x * 100
            print(f'{f.name}    {stat}:   {delta}%')

if __name__ == '__main__':
    main()
