from pathlib import Path
import subprocess
import gitlab

project_id = 1

def strip_prefix(s, prefix):
    if s.startswith(prefix):
        return s[len(prefix):]
    else:
        return None

def job_triple(job):
    return strip_prefix(job.name, 'validate-')

def fetch_artifacts(version: str, pipeline_id: int):
    gl = gitlab.Gitlab.from_config('ghc')
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

        print(job.name)
        #artifactZips = [ artifact
        #                 for artifact in job.artifacts
        #                 if artifact['filename'] == 'artifacts.zip' ]
        try:
            destdir = tmpdir / job.name
            zip_name = Path(f"{tmpdir}/{job.name}.zip")
            if not zip_name.exists():
                with open(zip_name, 'wb') as f:
                    job.artifacts(streamed=True, action=f.write)
            subprocess.run(['unzip', '-bo', zip_name, '-d', destdir])
            bindist = destdir / "ghc.tar.xz"
            if bindist.exists():
                dest = Path(f'ghc-{version}-{triple}.tar.xz')
                print(dest)
                bindist.replace(dest)
        except Exception as e:
            print(e)
            pass

def main():
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('--pipeline', '-p')
    parser.add_argument('--version', '-V')
    args = parser.parse_args()
    #fetch_artifacts('8.6.4', 1054)
    fetch_artifacts(args.version, args.pipeline)
