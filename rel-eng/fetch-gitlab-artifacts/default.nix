let
  nixpkgs = fetchGit {
    url = https://github.com/NixOS/nixpkgs;
    rev = "c8564b72a2bccb6f33d9d4dc074c86a526532242";
    ref = "release-19.03";
  };
in with (import nixpkgs {});
let
  fetch-gitlab = { buildPythonPackage, python-gitlab, unzip }:
    buildPythonPackage {
      pname = "fetch-gitlab";
      version = "0.0.1";
      src = nix-gitignore.gitignoreSource [] ./.;
      propagatedBuildInputs = [ python3Packages.python-gitlab unzip ];
      preferLocalBuild = true;
    };
in 
python3Packages.callPackage fetch-gitlab { inherit unzip; }
