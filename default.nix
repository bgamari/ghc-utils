let
  nixpkgs = fetchGit {
    url = https://github.com/NixOS/nixpkgs;
    rev = "ac29d96d25ac361a083a27f2f4a57f52d2817c20";
    #sha256 = "0000000000000000000000000000000000000000000000000000000000000000";
  };
in with (import nixpkgs {});
let
  fetch-gitlab = { buildPythonPackage, python-gitlab, unzip }:
    buildPythonPackage {
      pname = "fetch-gitlab";
      version = "0.0.1";
      src = ./.;
      propagatedBuildInputs = [ python3Packages.python-gitlab unzip ];
      preferLocalBuild = true;
    };
in 
python3Packages.callPackage fetch-gitlab { inherit unzip; }
