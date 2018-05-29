{ nixpkgs ? (import <nixpkgs> {}) }:

with nixpkgs;
rec {
  ghc-gdb = pythonPackages.buildPythonPackage {
    name = "ghc-gdb";
    src = ./.;
  };
  env = python.withPackages (_: [ ghc-gdb ]);
}

