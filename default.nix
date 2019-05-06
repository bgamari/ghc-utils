{ nixpkgs ? (import <nixpkgs> {}) }:

with nixpkgs;
let
  rel-eng =
    stdenv.mkDerivation {
      name = "rel-eng-scripts";
      nativeBuildInputs = [ makeWrapper ];
      buildCommand = ''
        mkdir -p $out/bin

        cp ${./debug-ghc} $out/bin/debug-ghc
        chmod ugo+rx $out/bin/debug-ghc
        substituteInPlace $out/bin/debug-ghc \
          --replace tempfile ${debianutils}/bin/tempfile \
          --replace 'PROG="gdb' 'PROG="${gdb.gdb}/bin/gdb'
        
        makeWrapper ${./parallel-rr.py} $out/bin/parallel-rr \
          --prefix PATH : ${gdb.rr}/bin
      '';
    };
  gdb = import ./gdb;
  gitlab-utils = import ./gitlab-utils;
in
  symlinkJoin {
    name = "hi";
    paths = [ gdb.gdb gdb.run-ghc-gdb gdb.run-ghc-rr gdb.dot2svg rel-eng gitlab-utils ];
  }
