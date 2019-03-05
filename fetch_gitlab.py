import logging
from pathlib import Path
import subprocess
import gitlab

project_id = 1

logging.basicConfig(level=logging.INFO)

def strip_prefix(s, prefix):
    if s.startswith(prefix):
        return s[len(prefix):]
    else:
        return None

def job_triple(job):
    bindists = {
        'validate-x86_64-darwin': 'x86_64-apple-darwin',
        'validate-i386-linux-deb9': 'i386-deb9-linux',
        'validate-x86_64-linux-deb8': 'x86_64-deb8-linux',
        'validate-x86_64-linux-deb9': 'x86_64-deb9-linux',
        'release-x86_64-linux-deb9-dwarf': 'x86_64-deb9-linux-dwarf',
        'validate-x86_64-windows': 'x86_64-unknown-mingw32',
        'validate-x86_64-linux-fedora27': 'x86_64-fedora27-linux',
    }
    if job.name in bindists:
        return bindists[job.name]
    else:
        return strip_prefix(job.name, 'validate-')

def fetch_artifacts(release: str, pipeline_id: int,
                    dest_dir: Path, gl: gitlab.Gitlab):
    dest_dir.mkdir(exist_ok=True)
    proj = gl.projects.get('ghc/ghc')
    pipeline = proj.pipelines.get(pipeline_id)
    tmpdir = Path("fetch-gitlab")
    tmpdir.mkdir(exist_ok=True)
    for pipeline_job in pipeline.jobs.list():
        if len(pipeline_job.artifacts) == 0:
            continue

        job = proj.jobs.get(pipeline_job.id)
        triple = job_triple(job)
        if triple is None:
            pass

        #artifactZips = [ artifact
        #                 for artifact in job.artifacts
        #                 if artifact['filename'] == 'artifacts.zip' ]
        try:
            destdir = tmpdir / job.name
            zip_name = Path(f"{tmpdir}/{job.name}.zip")
            if not zip_name.exists():
                with open(zip_name, 'wb') as f:
                    job.artifacts(streamed=True, action=f.write)

            if zip_name.stat().st_size == 0:
                logging.info(f'artifact archive for job {job.name} is empty')
                continue

            subprocess.run(['unzip', '-bo', zip_name, '-d', destdir])
            bindist = destdir / "ghc.tar.xz"
            if bindist.exists():
                dest = dest_dir / f'ghc-{release}-{triple}.tar.xz'
                logging.info(f'extracted {job.name} to {dest}')
                bindist.replace(dest)
            else:
                print('Bindist already exists')
        except Exception as e:
            logging.error(f'Error fetching job {job.name}: {e}')
            pass

def main():
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('--pipeline', '-p', required=True, type=int, help="pipeline id")
    parser.add_argument('--release', '-r', required=True, type=str, help="release name")
    parser.add_argument('--output', '-o', default=Path.cwd() / "out", help="output directory")
    parser.add_argument('--profile', '-P', default='haskell',
                        help='python-gitlab.cfg profile name')
    args = parser.parse_args()
    gl = gitlab.Gitlab.from_config(args.profile)
    fetch_artifacts(args.release, args.pipeline, 
                    dest_dir=args.output, gl=gl)
