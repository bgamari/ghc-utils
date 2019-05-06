#!/usr/bin/env python3

import subprocess
import threading
from pathlib import Path

def main(): 
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument('-j', '--jobs', type=int, default=1, help="Number of parallel jobs")
    parser.add_argument('cmdline', nargs='+', help="Command to run")
    args = parser.parse_args()

    stop = False
    base_path = Path('parallel-rr')
    base_path.mkdir(parents=True, exist_ok=True)
    n = 0
    def go():
        nonlocal n, stop
        while not stop:
            out_path = base_path / f"out-{n}"
            cmdline = ['rr', 'record', '-o', out_path] + args.cmdline
            p = subprocess.run(cmdline)
            if p.returncode != 0:
                print(out_path)
                stop = True
            else:
                out_path.rmtree()

            n += 1 


    threads = [ threading.Thread(target=go) for _ in range(args.jobs) ]
    for t in threads:
        t.start()
    for t in threads:
        t.join()


if __name__ == '__main__':
    main()
