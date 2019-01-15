from pathlib import Path
import subprocess
import gitlab

host = "https://gitlab.haskell.org"
root_url = f"{host}/api/v4"
token = 'MakRrr7JYo7wTqE1zT3x' # personal access token
headers = { 'Private-Token': token }
project_id = 1

def fetch_job(job_id: int):
    #resp = requests.get(f'{root_url}/projects/{project_id}/jobs/{job_id}/artifacts', headers=headers).json()
    print(resp)

def fetch_artifacts(pipeline_id: int):
    gl = gitlab.Gitlab(host, private_token=token)
    proj = gl.projects.get('ghc/ghc')
    pipeline = proj.pipelines.get(pipeline_id)
    for pipeline_job in pipeline.jobs.list():
        if len(pipeline_job.artifacts) == 0:
            continue
        job = proj.jobs.get(pipeline_job.id)
        print(job.name)
        #artifactZips = [ artifact
        #                 for artifact in job.artifacts
        #                 if artifact['filename'] == 'artifacts.zip' ]
        try:
            tmpdir = Path("fetch-gitlab")
            tmpdir.mkdir(exist_ok=True)
            zip_name = f"{tmpdir}/{job.name}.zip"
            with open(zip_name, 'wb') as f:
                job.artifacts(streamed=True, action=f.write)
            subprocess.run(['unzip', '-bo', zip_name])
            print(job.artifacts)
        except Exception as e:
            print(e)
            pass

fetch_artifacts(1054)
