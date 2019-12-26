#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
This is a handy tool for listing the state of GHC's submodules and their
release state.

In the case that a submodule differs only in benign (e.g. only via the .cabal
file) ways one may push an annotated tag of the form `v1.2.3-ghc1` to the
package's gitlab.haskell.org mirror to indicate that the change is acceptable
in a release (but do make sure that the changes make it only Hackage as a
revision is made).
"""

import subprocess
import re
from pathlib import Path
from typing import List, Tuple

# Things that aren't releasable
non_released = {
    '.arc-linters/arcanist-external-json-linter',
    'nofib',
    'libffi-tarballs',
    'libraries/integer-gmp/gmp/gmp-tarballs'
}

# Things that aren't in the bindist package database
non_installed = {
    'libraries/parallel'
}

# Things that GHC HQ controls
ghc_maintained = {
    'libraries/hpc',
}

def is_release_tag(tag: str) -> bool:
    tag = tag.replace('_', '.') # for transformers
    m = re.match(r'(?:[a-zA-Z]+-|v)?([0-9]+(\.[0-9]+)*((-release)?|-r[0-9]+?|(-ghc[0-9]+)?))$', tag)
    if m is not None:
        return m.group(1)
    else:
        return None

def list_submodules(repo: Path) -> List[Tuple[Path, str]]:
    p = subprocess.run(['git', 'submodule'], capture_output=True, encoding='UTF-8')
    result = []
    for line in p.stdout.split('\n'):
        parts = line.split()
        if len(parts) == 0:
            continue
        path = parts[1]
        rev = parts[2]
        result.append((path, rev))

    return result

def main() -> None:
    #subprocess.run(['git', 'submodule', 'foreach', 'git', 'remote', 'update', 'upstream'], check=True)
    for path, rev in list_submodules('.'):
        if path in non_released \
                or path in ghc_maintained \
                or path in non_installed:
            continue
        tag = re.match('\((.*)\)', rev).group(1)
        version = is_release_tag(tag)
        print('   * [{mark}] `{path}`: {version}'.format(
            mark = 'x' if version is not None else ' ',
            path = path,
            version = f'version {version}' if version is not None else f'*todo* (on `{tag}`)'
        ))


if __name__ == '__main__':
    main()
