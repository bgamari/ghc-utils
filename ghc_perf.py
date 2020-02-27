#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from math import sqrt
from typing import List, Tuple, Dict, NewType, Optional, TextIO, DefaultDict
import typing
from pathlib import Path
import io
import tempfile
import subprocess
import json

Label = NewType('Label', List[str])

DEFAULT_EVENTS = 'cycles instructions cache-misses branches branch-misses'.split()

def parse_label(lbl: str) -> Label:
    return Label(lbl.split('//'))

def pack_label(lbl: Label) -> str:
    return '//'.join(lbl)

class Metrics:
    metrics: List[Tuple[Label, float]]

    def __init__(self, metrics: List[Tuple[Label, float]]):
        self.metrics = metrics

    @staticmethod
    def read_tsv(f: TextIO) -> "Metrics":
        import csv
        reader = csv.reader(f, delimiter='\t')
        return Metrics([ (parse_label(lbl), float(v)) for (lbl, v) in reader ])

    def write_tsv(self, f: TextIO):
        import csv
        writer = csv.writer(f, delimiter='\t')
        for lbl, v in self.metrics:
            writer.writerow((pack_label(lbl), v))

    @staticmethod
    def read_json(f: TextIO) -> "Metrics":
        return Metrics(json.load(f))

    def write_json(self, f: TextIO) -> None:
        json.dump(self.metrics, f, indent=2)

    def __add__(self, other: "Metrics") -> "Metrics":
        return Metrics(self.metrics + other.metrics)

    def __getitem__(self, lbl: Label) -> List[float]:
        return [v for l,v in self.metrics if lbl == l]

    def grouped(self) -> Dict[Label, List[float]]:
        import collections
        grouped = collections.defaultdict(lambda: []) # type: DefaultDict[Label, List[float]]
        for k,v in self.metrics:
            grouped[k].append(v)

        return grouped

    def add(self, lbl: Label, value: float) -> None:
        self.metrics.append((lbl, value))


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
        ) -> Metrics:

    metrics = Metrics([]) # type: Metrics
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
        for k,v in rts_stats.items():
            metrics.add(Label([name] + ['run', 'rts', k]), v)

        if events is not None:
            perf_stats = read_perf_stats(perf_out)
            for k,v in perf_stats.items():
                metrics.add(Label([name] + ['run', 'perf', k]), v)

    return metrics

def main() -> None:
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('-n', '--name', type=str,
                        help='test name')
    parser.add_argument('-o', '--output', type=str,
                        help='output file name (default: PROG.perf.json)')
    parser.add_argument('-a', '--append', action='store_true',
                        help='append rather than overwrite output file')
    parser.add_argument('-r', '--repeat', type=int, default=1,
                        help='run N times')
    parser.add_argument('-e', '--events', type=str, nargs='*', default=[],
                        help='perf events to count (in addition to default set, if --perf is given)')
    parser.add_argument('-E', '--perf', action='store_true',
                        help='shorthand for --events="{}"'.format(','.join(DEFAULT_EVENTS)))
    parser.add_argument('-s', '--summarize', action='store_true',
                        help='show summary after run')
    parser.add_argument('--stdin', type=argparse.FileType('rb'),
                        help='take stdin from given file')
    parser.add_argument('cmdline', nargs='+',
                        help='command line to run')
    args = parser.parse_args()

    output_path = Path('{}.perf.json'.format(args.cmdline[0]))
    if args.output is not None:
        output_path = Path(args.output)

    if args.append and output_path.is_file():
        if output_path.suffix == '.json':
            old_metrics = Metrics.read_json(output_path.open())
        else:
            old_metrics = Metrics.read_tsv(output_path.open())
    else:
        old_metrics = Metrics([])

    events = []  # type: List[str]
    events += args.events
    if args.perf:
        events += DEFAULT_EVENTS

    metrics = run(
        name=args.name if args.name else Path(args.cmdline[0]).name,
        cmdline=args.cmdline,
        events=events,
        repeat=args.repeat,
        stdin=Path(args.stdin.name) if args.stdin else None)

    new_metrics = old_metrics + metrics
    if args.output.endswith('.json'):
        new_metrics.write_json(output_path.open('w'))
    else:
        new_metrics.write_tsv(output_path.open('w'))

    if args.summarize:
        print("")
        print(f"Summary ({args.repeat} repetitions)")
        print( "====================================")
        print("")
        for k,vs in sorted(metrics.grouped().items()):
            mu = mean(vs)
            stderr = std(vs) / sqrt(len(vs)) / mu * 100 if mu != 0 else 0
            print(f'{k:60s}   {mu:<8.2g} +/- {stderr:>4.1f}%')

    print("")
    print(f'Metrics from {args.repeat} repetitions saved in {output_path}')

def std(xs: List[float]) -> float:
    mu = mean(xs)
    return sqrt(mean([(x-mu)**2 for x in xs]))

def mean(xs: List[float]) -> float:
    return sum(xs) / len(xs)

if __name__ == '__main__':
    main()
