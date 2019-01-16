{ nixpkgs ? (import <nixpkgs> {}) }:

with nixpkgs;
let
  fetch-gitlab = { buildPythonPackage, python-gitlab, unzip }:
    buildPythonPackage {
      pname = "fetch-gitlab";
      version = "0.0.1";
      src = ./.;
      propagatedBuildInputs = [ python3Packages.python-gitlab unzip ];
    };
in
python3Packages.callPackage fetch-gitlab { inherit unzip; }
