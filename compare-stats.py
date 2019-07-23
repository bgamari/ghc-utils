"""
Utility for comparing output from GHC's +RTS -t --machine-readable output.
"""

import re
import sys
import ast

def read_rts_stats(f) -> dict:
    f.readline()
    raw = ast.literal_eval(re.sub('^ +', '', f.read()))
    parsed = { key: float(value) for key, value in raw }
    return parsed

def main() -> None:
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('-s', '--stat', type=str, action='append', default=[])
    parser.add_argument('-o', '--output', type=argparse.FileType('w'), default=sys.stdout,
                        help='')
    parser.add_argument('reference', type=argparse.FileType('r'))
    parser.add_argument('inputs', type=argparse.FileType('r'), nargs='+',
                        help='')
    args = parser.parse_args()

    stats = args.stat
    if stats == []:
        stats = ['allocated_bytes', 'total_cpu_seconds']

    ref = read_rts_stats(args.reference)
    for stat in stats:
        print(f'{args.reference.name}    {stat}:    {ref[stat]}')

    for f in args.inputs:
        parsed = read_rts_stats(f)
        for stat in stats:
            x = ref[stat]
            y = parsed[stat]
            delta = (y - x) / x * 100
            print(f'{f.name}    {stat}:   {delta}%')

if __name__ == '__main__':
    main()
