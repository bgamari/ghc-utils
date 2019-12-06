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
        'validate-aarch64-linux-deb9': 'aarch64-deb9-linux',
        'validate-armv7-linux-deb9': 'armv7-deb9-linux',
        'validate-x86_64-darwin': 'x86_64-apple-darwin',
        'validate-i386-linux-deb9': 'i386-deb9-linux',
        'release-x86_64-linux-deb8': 'x86_64-deb8-linux',
        'validate-x86_64-linux-deb9': 'x86_64-deb9-linux',
        'release-x86_64-linux-deb9': 'x86_64-deb9-linux',
        'release-x86_64-linux-deb9-dwarf': 'x86_64-deb9-linux-dwarf',
        'release-x86_64-windows': 'x86_64-unknown-mingw32',
        'release-i386-windows': 'i386-unknown-mingw32',
        #'validate-x86_64-windows': 'x86_64-unknown-mingw32',
        #'validate-i386-windows': 'i386-unknown-mingw32',
        'validate-x86_64-linux-fedora27': 'x86_64-fedora27-linux',
        'release-x86_64-linux-centos7': 'x86_64-centos7-linux',
        'source-tarball': 'src',
    }
    if job.name in bindists:
        return bindists[job.name]
    else:
        #return strip_prefix(job.name, 'validate-')
        return None

def fetch_artifacts(release: str, pipeline_id: int,
                    dest_dir: Path, gl: gitlab.Gitlab):
    dest_dir.mkdir(exist_ok=True)
    proj = gl.projects.get('ghc/ghc')
    pipeline = proj.pipelines.get(pipeline_id)
    tmpdir = Path("fetch-gitlab")
    tmpdir.mkdir(exist_ok=True)
    for pipeline_job in pipeline.jobs.list(all=True):
        if len(pipeline_job.artifacts) == 0:
            logging.info(f'job {pipeline_job.name} ({pipeline_job.id}) has no artifacts')
            continue

        job = proj.jobs.get(pipeline_job.id)
        triple = job_triple(job)
        if triple is None:
            logging.info(f'ignoring {job.name}')
            continue

        #artifactZips = [ artifact
        #                 for artifact in job.artifacts
        #                 if artifact['filename'] == 'artifacts.zip' ]
        try:
            destdir = tmpdir / job.name
            zip_name = Path(f"{tmpdir}/{job.name}.zip")
            if not zip_name.exists() or zip_name.stat().st_size == 0:
                logging.info(f'downloading archive {zip_name} for job {job.name} (job {job.id})...')
                with open(zip_name, 'wb') as f:
                    job.artifacts(streamed=True, action=f.write)

            if zip_name.stat().st_size == 0:
                logging.info(f'artifact archive for job {job.name} (job {job.id}) is empty')
                continue

            dest = dest_dir / f'ghc-{release}-{triple}.tar.xz'
            if dest.exists():
                logging.info(f'bindist {dest} already exists')
                continue

            subprocess.run(['unzip', '-bo', zip_name, '-d', destdir])
            bindist_files = list(destdir.glob('ghc*.tar.xz'))
            if len(bindist_files) == 0:
                logging.warn(f'Bindist {bindist} does not exist')
                continue

            if job.name == 'source-tarball':
                for f in bindist_files:
                    dest = dest_dir / f.name
                    logging.info(f'extracted {job.name} to {dest}')
                    f.replace(dest)
            else:
                bindist = bindist_files[0]
                logging.info(f'extracted {job.name} to {dest}')
                bindist.replace(dest)
        except Exception as e:
            logging.error(f'Error fetching job {job.name}: {e}')
            pass

def main():
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('--pipeline', '-p', required=True, type=int, help="pipeline id")
    parser.add_argument('--release', '-r', required=True, type=str, help="release name")
    parser.add_argument('--output', '-o', type=Path, default=Path.cwd() / "out", help="output directory")
    parser.add_argument('--profile', '-P', default='haskell',
                        help='python-gitlab.cfg profile name')
    args = parser.parse_args()
    gl = gitlab.Gitlab.from_config(args.profile)
    fetch_artifacts(args.release, args.pipeline, 
                    dest_dir=args.output, gl=gl)
