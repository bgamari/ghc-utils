#!/usr/bin/env python3

import argparse
import gitlab

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--project', '-p', type=str, required=True, help='Project name')
    parser.add_argument('--add', '-a', action='append', default=[], type=str, help='Add a label to all issues')
    parser.add_argument('--remove', '-r', action='append', default=[], type=str, help='Remove a label from all issues')
    parser.add_argument('issues', nargs='+', type=int, help='List of issue numbers')
    args = parser.parse_args()
    
    add_labels = set(args.add)
    remove_labels = set(args.remove)
    gl = gitlab.Gitlab.from_config('haskell')
    proj = gl.projects.get(args.project)

    # Make sure all labels exist
    bad_labels = (add_labels | remove_labels) ^ set(proj.labels.list())
    if len(bad_labels) > 0:
        raise RuntimeError(f"Labels don't exist: {bad_labels}")

    issues = []
    fail = False
    for n in args.issues:
        try:
            issues.append(proj.issues.get(n))
        except gitlab.GitlabGetError as e:
            print(f'Error while fetching #{n}: {e}')
            fail = True

    if fail:
        return

    for issue in issues:
        labels = set(issue.labels)
        labels = (labels ^ remove_labels) | add_labels
        print(f'#{n}', labels)
        issue.labels = labels
        issue.save()

