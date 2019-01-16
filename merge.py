#!/usr/bin/env python3

from typing import NewType, NamedTuple, Dict
import subprocess
from subprocess import run, PIPE
from argparse import ArgumentParser
from pathlib import Path
import json

STATE_FILE = Path('.merge.json')

Rev = NewType('Rev', str)
HEAD = Rev('HEAD')
CommitSha = NewType('CommitSha', str)

class MergedMR(NamedTuple):
    commit: CommitSha

def main() -> None:
    parser = ArgumentParser()
    subparsers = parser.add_subparsers()
    subparser = subparsers.add_parser('merge', help='land a commit', aliases=['land'])
    subparser.add_argument('merge_request', type=int, help='merge request to merge')
    subparser.add_argument('--squash', '-s', action='store_true',
                           help='Squash commits')
    subparser.set_defaults(mode='merge')

    subparser = subparsers.add_parser('show-mr-message', help='Print merge request message')
    subparser.set_defaults(mode='show_mr_message')

    subparser = subparsers.add_parser('finish', help='finish')
    subparser.set_defaults(mode='finish')

    subparser = subparsers.add_parser('reset', help='finish')
    subparser.set_defaults(mode='reset')

    args = parser.parse_args()
    if args.mode == 'merge':
        merge(args.merge_request, args.squash)
    elif args.mode == 'show_mr_message':
        show_mr_message()
    elif args.mode == 'finish':
        finish()
    elif args.mode == 'reset':
        reset()
    else:
        raise "uh oh"

def load_state() -> Dict[int, MergedMR]:
    if STATE_FILE.exists():
        return json.load(STATE_FILE.open())
    else:
        return {}

def reset() -> None:
    STATE_FILE.unlink()
    run(['git', 'checkout', 'wip/merge-queue'])
    run(['git', 'reset', '--hard', 'origin/master'])

def show_mr_message():
    state = load_state()
    mr_list = '\n'.join(f' * !{mr}' for mr in state)
    msg = f'''
    Bulk merge branch merging the following merge requests:

    {mr_list}
    '''
    import textwrap
    print(textwrap.dedent(msg))

def finish() -> None:
    import gitlab
    gl = gitlab.Gitlab.from_config('ghc')
    proj = gl.projects.get('ghc/ghc')
    merged = load_state()
    for mr_num, merged_mr in merged.items():
        mr = proj.mergerequests.get(mr_num)
        commit = merged_mr.commit
        mr.discussions.notes.create({'body': f'Merged in {commit}'})

        mr.state_event = 'close'
        mr.save()
        print(f'Closed {mr.iid}')

def rev_parse(rev: Rev) -> CommitSha:
    commit = run(['git', 'rev-parse', rev], stdout=PIPE)
    return CommitSha(commit.stdout.decode('UTF-8').strip())

def squash_from(rev: Rev) -> CommitSha:
    commit_msg = run(['git', 'log', '--format=%B', '--reverse', f'{rev}..HEAD'],
                     stdout=PIPE).stdout
    run(['git', 'reset', '--soft', 'wip/merge-queue'])
    run(['git', 'commit', '--edit', '-m', commit_msg])
    return rev_parse(HEAD)

def merge(mr: int, squash: bool) -> None:
    merged = load_state()

    if mr in merged:
        commit = merged[mr].commit
        print(f"Merge request !{mr} already merged with commit {commit}.")
        return

    run(['git', 'fetch', 'origin', f'merge-requests/{mr}/head'])
    run(['git', 'checkout', 'FETCH_HEAD'])
    run(['git', 'rebase', 'wip/merge-queue'])
    if squash:
        commit = squash_from(Rev('wip/merge-queue'))
    else:
        commit = rev_parse(HEAD)
    run(['git', 'checkout', 'wip/merge-queue'])
    run(['git', 'merge', commit])

    merged[mr] = MergedMR(commit=commit)
    json.dump(merged, STATE_FILE.open('w'))
    print(f'Merged !{mr}.')

if __name__ == '__main__':
    main()
