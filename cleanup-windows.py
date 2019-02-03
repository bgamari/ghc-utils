#!/usr/bin/python3

"""
This script deletes build working directories older than the given maximum age.

It should be scheduled to run periodically on Windows runners with
the --register flag.
"""

registration_script = r"""
$A = New-ScheduledTaskAction `
    -Execute "C:\msys64\mingw64\bin\python3" `
    -Argument "C:\msys64\home\bgamari_ghc\cleanup-windows.py"
$S = New-TimeSpan -Hour 6
$T = New-ScheduledTaskTrigger -RepetitionInterval $S -At 3am -Once
Register-ScheduledTask `
    -User "gitlab" `
    -Password {password} `
    -TaskName "GitLab Cleanup" `
    -Trigger $T `
    -Action $A
"""

unregistration_script = r"""
Unregister-ScheduledTask -TaskName "GitLab Cleanup"
"""

import shutil
from pathlib import Path
from datetime import datetime, timedelta
from time import time
from subprocess import run
import os
import stat
import logging
from logging.handlers import NTEventLogHandler

MAX_AGE = timedelta(hours=8)
GITLAB_DIR = Path(r'C:\GitLabRunner')
POWERSHELL_PATH =  r"C:\WINDOWS\system32\WindowsPowerShell\v1.0\powershell.exe"

#logging.basicConfig(handlers=[NTEventLogHandler('gitlab-cleanup')])
logging.basicConfig(level=logging.INFO)

def run_powershell(script: str):
    import tempfile
    with tempfile.NamedTemporaryFile('w', suffix='.ps1', delete=False) as f:
        f.write(script)
        f.close()
        run([POWERSHELL_PATH, '-File', f.name], check=True)
        os.unlink(f.name)

def robust_rm(path: Path):
    def on_error(fn, path2, excinfo):
        os.chmod(path2, stat.S_IWRITE)
        os.remove(path2)

    shutil.rmtree(path, onerror=on_error)

def clean(max_age: timedelta, dry_run: bool):
    # Clean cache
    for d in (GITLAB_DIR / 'cache').iterdir():
        try:
            logging.info(f'deleting {d}')
            robust_rm(d)
        except Exception as e:
            logging.error(f'Error while deleting {d}: {e}')

    # Clean builds
    for runner_dir in (GITLAB_DIR / 'builds').iterdir():
        for inst_dir in runner_dir.iterdir():
            for user_dir in inst_dir.iterdir():
                try:
                    ghc_dir = user_dir / 'ghc'
                    s = ghc_dir.stat()
                    mtime = datetime.fromtimestamp(s.st_mtime)
                    age = datetime.now() - mtime
                    delete = age > max_age

                    action = 'deleting' if delete else 'keeping '
                    logging.info(f'{action} {user_dir} (age={age})')
                    if delete and not dry_run:
                        robust_rm(user_dir)
                except Exception as e:
                    logging.error(f'Error while deleting {user_dir}: {e}')

def main():
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('--register', action='store_true')
    parser.add_argument('--password', type=str)
    parser.add_argument('--unregister', action='store_true')
    parser.add_argument('-n', '--dry-run', action='store_true')
    parser.add_argument('--max-age', default=timedelta(hours=8))
    args = parser.parse_args()

    if args.register:
        if args.password is None:
            raise RuntimeError("--password is required")
        run_powershell(registration_script.format(password=args.password))
    elif args.unregister:
        run_powershell(unregistration_script)
    else:
        clean(max_age=args.max_age, dry_run=args.dry_run)

if __name__ == '__main__':
    main()

