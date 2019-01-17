#!/usr/bin/env python3

from typing import NewType, NamedTuple, Dict, List
import subprocess
from subprocess import run, PIPE
from argparse import ArgumentParser
from pathlib import Path
import pickle

STATE_FILE = Path('.merge.pickle')

Rev = NewType('Rev', str)
HEAD = Rev('HEAD')
CommitSha = NewType('CommitSha', str)

class MergedMR(NamedTuple):
    mr_id: int
    commit: CommitSha

class State(NamedTuple):
    merged_mrs: List[MergedMR]

def main() -> None:
    parser = ArgumentParser()
    subparsers = parser.add_subparsers()
    subparser = subparsers.add_parser('merge', help='land a commit', aliases=['land'])
    subparser.add_argument('merge_request', type=int, help='merge request to merge')
    subparser.add_argument('--squash', '-s', action='store_true',
                           help='Squash commits')
    subparser.set_defaults(mode='merge')

    subparser = subparsers.add_parser('rollback', help='Rollback the last merge')
    subparser.set_defaults(mode='rollback')

    subparser = subparsers.add_parser('show-mr-message', help='Print merge request message')
    subparser.set_defaults(mode='show_mr_message')

    subparser = subparsers.add_parser('finish', help='finish')
    subparser.set_defaults(mode='finish')

    subparser = subparsers.add_parser('reset', help='finish')
    subparser.set_defaults(mode='reset')

    args = parser.parse_args()
    if args.mode == 'merge':
        merge(args.merge_request, args.squash)
    elif args.mode == 'rollback':
        rollback()
    elif args.mode == 'show_mr_message':
        show_mr_message()
    elif args.mode == 'finish':
        finish()
    elif args.mode == 'reset':
        reset()
    else:
        raise "uh oh"

def load_state() -> State:
    if STATE_FILE.exists():
        return pickle.load(STATE_FILE.open('rb'))
    else:
        return State(merged_mrs=[])

def save_state(state: State) -> None:
    pickle.dump(state, STATE_FILE.open('wb'))

def reset() -> None:
    print('Resetting state...')
    if STATE_FILE.exists():
        STATE_FILE.unlink()
    run(['git', 'checkout', 'wip/merge-queue'], check=True)
    run(['git', 'reset', '--hard', 'origin/master'], check=True)

def rollback() -> None:
    state = load_state()
    if len(state.merged_mrs) > 1:
        last_mr = state.merged_mrs.pop()
        print(f'Rolling back !{last_mr.mr_id}...')
        commit = state.merged_mrs[-2].commit
        run(['git', 'reset', '--hard', commit])
        save_state(state)
    else:
        reset()

def show_mr_message():
    state = load_state()
    mr_list = '\n'.join(f' * !{mr.mr_id} with {mr.commit}' for mr in state.merged_mrs)
    msg = f'''Bulk merge branch merging the following merge requests:

    {mr_list}
    '''
    import textwrap
    print(textwrap.dedent(msg))

def finish() -> None:
    import gitlab
    gl = gitlab.Gitlab.from_config('ghc')
    proj = gl.projects.get('ghc/ghc')
    state = load_state()
    for mmr in state.merged_mrs:
        mr = proj.mergerequests.get(mmr.mr_id)
        mr.discussions.notes.create({'body': f'Merged in {mmr.commit}'})

        mr.state_event = 'close'
        mr.save()
        print(f'Closed {mr.iid}')

def rev_parse(rev: Rev) -> CommitSha:
    commit = run(['git', 'rev-parse', rev], check=True, stdout=PIPE)
    return CommitSha(commit.stdout.decode('UTF-8').strip())

def squash_from(rev: Rev) -> CommitSha:
    commit_msg = run(['git', 'log', '--format=%B', '--reverse', f'{rev}..HEAD'],
                     check=True, stdout=PIPE).stdout
    run(['git', 'reset', '--soft', 'wip/merge-queue'], check=True)
    run(['git', 'commit', '--edit', '-m', commit_msg], check=True)
    return rev_parse(HEAD)

def merge(mr: int, squash: bool) -> None:
    state = load_state()

    merged_mrs = { mmr.mr_id for mmr in state.merged_mrs } # type: Dict[int, MergedMR]

    if mr in merged_mrs:
        commit = merged_mrs[mr].commit
        print(f"Merge request !{mr} already merged with commit {commit}.")
        return

    run(['git', 'fetch', 'origin', f'merge-requests/{mr}/head'], check=True)
    run(['git', 'checkout', 'FETCH_HEAD'], check=True)
    run(['git', 'rebase', 'wip/merge-queue'], check=True)
    if squash:
        commit = squash_from(Rev('wip/merge-queue'))
    else:
        commit = rev_parse(HEAD)
    run(['git', 'checkout', 'wip/merge-queue'], check=True)
    run(['git', 'merge', commit], check=True)

    state.merged_mrs.append(MergedMR(mr_id=mr, commit=commit))
    save_state(state)
    print(f'Merged !{mr}.')

if __name__ == '__main__':
    main()
