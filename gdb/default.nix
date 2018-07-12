# Usage:  nix build gdbinit
#
# `source` the resulting file in gdb.

#{ nixpkgs ? (import <nixpkgs> {}) }:

let
  rev = "140ad12d71c57716b3ee3b777d53c27b019360f0";
  baseNixpkgs =
    builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = "088wzwgm7nqjv2bpirmjsncysjvzjd237z7d0lm2jzng81f4a6ll";
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

