# Usage:  nix build gdbinit
#
# `source` the resulting file in gdb.

#{ nixpkgs ? (import <nixpkgs> {}) }:

let
  rev = "08d245eb31a3de0ad73719372190ce84c1bf3aee";
  baseNixpkgs =
    builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = "1g22f8r3l03753s67faja1r0dq0w88723kkfagskzg9xy3qs8yw8";
  };
  nixpkgs = import baseNixpkgs {};

in with nixpkgs; rec {
  pythonPackages = python3Packages;

  ghc-gdb = pythonPackages.buildPythonPackage {
    name = "ghc-gdb";
    doCheck = false;
    src = ./.;
  };

  gdb = nixpkgs.gdb.override {
    python = python3;
  };

  env = python.withPackages (_: [ ghc-gdb ]);

  gdbinit = writeTextFile {
    name = "gdbinit";
    text = ''
      python sys.path += ["${env}/lib/python2.7/site-packages"]
      python import ghc_gdb
      echo The `ghc` command is now available.\n
    '';
  };
}

