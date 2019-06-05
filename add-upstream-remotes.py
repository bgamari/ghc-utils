#!/usr/bin/env python3

from pathlib import Path
import subprocess

IGNORE = 1 # ignore submodule
GITHUB_HASKELL = 2 # in the haskell github org
ORIGIN = 3 # upstream remote == origin remote

def github(owner, name):
    return f'https://github.com/{owner}/{name}'

def github_haskell(name):
    return github('haskell', name)

upstreams = {
    '.arc-linters/arcanist-external-json-linter': IGNORE,
    'libffi-tarballs': IGNORE,
    'libraries/array': ORIGIN,
    'libraries/binary': github('kolmodin', 'binary'),
    'libraries/bytestring': GITHUB_HASKELL,
    'libraries/Cabal': GITHUB_HASKELL,
    'libraries/containers': GITHUB_HASKELL,
    'libraries/deepseq': GITHUB_HASKELL,
    'libraries/directory': GITHUB_HASKELL,
    'libraries/filepath': GITHUB_HASKELL,
    'libraries/haskeline': github('judah', 'haskeline'),
    'libraries/hpc': ORIGIN,
    'libraries/integer-gmp/gmp/gmp-tarballs': ORIGIN,
    'libraries/mtl': GITHUB_HASKELL,
    'libraries/parallel': GITHUB_HASKELL,
    'libraries/parsec': github('hvr', 'parsec'),
    'libraries/pretty': GITHUB_HASKELL,
    'libraries/primitive': GITHUB_HASKELL,
    'libraries/process': GITHUB_HASKELL,
    'libraries/stm': GITHUB_HASKELL,
    'libraries/terminfo': github('judah', 'terminfo'),
    'libraries/text': GITHUB_HASKELL,
    'libraries/time': GITHUB_HASKELL,
    'libraries/transformers': IGNORE, # darcs mirror
    'libraries/unix': GITHUB_HASKELL,
    'libraries/Win32': GITHUB_HASKELL,
    'nofib': 'https://gitlab.haskell.org/ghc/nofib',
    'utils/hpc': GITHUB_HASKELL,
    'utils/haddock': GITHUB_HASKELL,
}

all_submods = [ 
    line.split()[1]
    for line in subprocess.check_output(['git', 'submodule'], encoding='UTF-8').split('\n')
    if len(line.split()) > 0
]

packages = {
    line.split()[0]: line.split()[3]
    for line in open('packages').read().split('\n')
    if not line.startswith('#')
    if len(line.split()) == 4
    if line.split()[3] != '-'
}

for submod in all_submods:
    print(submod)
    upstream = None
    if submod in upstreams:
        upstream = upstreams[submod]
    elif submod in packages:
        upstream = packages[submod]

    if upstream == ORIGIN:
        upstream = subprocess.check_output(['git', '-C', submod, 'remote', 'get-url', 'origin'], encoding='UTF-8').strip()
    elif upstream == GITHUB_HASKELL:
        upstream = github_haskell(Path(submod).name)
    elif upstream == IGNORE:
        continue

    if upstream is None:
        print(f'Unknown upstream for {submod}')
        raise ValueError('unknown upstream')
    else:
        print(f'Upstream of {submod} is {upstream}')
        p = subprocess.run(['git', '-C', submod, 'remote', 'get-url', 'upstream'], encoding='UTF-8', stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
        if p.returncode == 0:
            if p.stdout == upstream:
                continue
            else:
                subprocess.call(['git', '-C', submod, 'remote', 'set-url', 'upstream', upstream])
        else:
            subprocess.call(['git', '-C', submod, 'remote', 'add', 'upstream', upstream])

        subprocess.check_call(['git', '-C', submod, 'remote', 'update', 'upstream'])

