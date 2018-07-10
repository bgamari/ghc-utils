# Usage:  nix build gdbinit
#
# `source` the resulting file in gdb.

{ nixpkgs ? (import <nixpkgs> {}) }:

with nixpkgs;
rec {
  ghc-gdb = pythonPackages.buildPythonPackage {
    name = "ghc-gdb";
    doCheck = false;
    src = ./.;
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

