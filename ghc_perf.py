#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from math import sqrt
from typing import List, Tuple, Dict, NewType, Optional
import typing
from pathlib import Path
import io
import tempfile
import subprocess
import json

Label = NewType('Label', List[str])

DEFAULT_EVENTS = 'cycles instructions cache-misses branches branch-misses'.split()

def read_rts_stats(f: typing.TextIO) -> Dict[str, float]:
    """ shamelessly copied from rts_stats.py """
    import ast
    import re
    f.readline()
    raw = ast.literal_eval(re.sub('^ +', '', f.read()))
    parsed = { key: float(value) for key, value in raw }
    return parsed

def read_perf_stats(f: typing.TextIO) -> Dict[str, float]:
    import csv
    out = {}
    for row in csv.reader(f):
        if row == []:
            continue
        if row[0].startswith('#'):
            continue

        metric = row[2]
        value = row[0]
        if value == '<not supported>':
            continue

        out[metric] = float(value)

    return out

def run(name: str,
        cmdline: List[str],
        events: List[str] = [],
        repeat: int = 1,
        stdin: Optional[Path] = None
        ) -> List[Tuple[Label,float]]:

    metrics = [] # type: List[Tuple[Label,float]]
    for i in range(repeat):
        rts_out = tempfile.NamedTemporaryFile(mode='r')
        perf_out = tempfile.NamedTemporaryFile(mode='r')

        cmd = cmdline + \
            ['+RTS', '--machine-readable', '-t'+rts_out.name, '-RTS']

        # Add perf invocation if requested
        if events:
            cmd = ['perf', 'stat', '-x,', '-o', perf_out.name] + \
                ['-e', ','.join(events)] + ['--'] + cmd

        #print(cmd)
        subprocess.check_call(cmd,
                              stdin=stdin.open('rb') if stdin else None)

        rts_stats = read_rts_stats(rts_out)
        metrics += [ (Label([name] + ['run', 'rts', k]), v)
                     for k,v in rts_stats.items()
                   ]
        if events is not None:
            perf_stats = read_perf_stats(perf_out)
            metrics += [ (Label([name] + ['run', 'perf', k]), v)
                         for k,v in perf_stats.items()
                       ]

    return metrics

def main() -> None:
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('-n', '--name', type=str,
                        help='test name')
    parser.add_argument('-o', '--output', type=argparse.FileType('w'),
                        help='output file name (default: PROG.perf.json)')
    parser.add_argument('-r', '--repeat', type=int, default=1,
                        help='run N times')
    parser.add_argument('-E', '--perf', action='store_true',
                        help='run under perf with default event set ({})'.format(DEFAULT_EVENTS))
    parser.add_argument('-e', '--events', type=str, nargs='*', default=[],
                        help='perf events to count (in addition to default set, if --perf is given)')
    parser.add_argument('-s', '--summarize', action='store_true',
                        help='show summary after run')
    parser.add_argument('--stdin', type=argparse.FileType('rb'),
                        help='take stdin from given file')
    parser.add_argument('cmdline', nargs='+',
                        help='command line to run')
    args = parser.parse_args()

    output = args.output
    if output is None:
        output = open('{}.perf.json'.format(args.cmdline[0]), 'w')

    events = []
    events += args.events
    if args.perf:
        events += DEFAULT_EVENTS

    metrics = run(
        name=args.name if args.name else args.cmdline[0],
        cmdline=args.cmdline,
        events=events,
        repeat=args.repeat,
        stdin=Path(args.stdin.name) if args.stdin else None)

    json.dump(metrics, output, indent=2)

    if args.summarize:
        print("")
        print(f"Summary ({args.repeat} repetitions)")
        print( "====================================")
        print("")
        for k,vs in sorted(group_metrics(metrics).items()):
            mu = mean(vs)
            stderr = std(vs) / sqrt(len(vs)) / mu * 100 if mu != 0 else 0
            print(f'{k:60s}   {mu:<8.2g} +/- {stderr:>4.1f}%')

    print("")
    print(f'Metrics from {args.repeat} repetitions saved in {output.name}')

def std(xs: List[float]) -> float:
    mu = mean(xs)
    return sqrt(mean([(x-mu)**2 for x in xs]))

def mean(xs: List[float]) -> float:
    return sum(xs) / len(xs)

def group_metrics(metrics: List[Tuple[Label, float]]) -> Dict[Label, List[float]]:
    import collections
    grouped = collections.defaultdict(lambda: [])
    for k,v in metrics:
        name = '//'.join(k)
        grouped[name].append(v)

    return grouped

if __name__ == '__main__':
    main()
