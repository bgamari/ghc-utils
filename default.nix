{ nixpkgs ? (import <nixpkgs> {}) }:

with nixpkgs;
let
  misc-scripts =
    stdenv.mkDerivation {
      name = "misc-scripts";
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
  gdb = import ./gdb { inherit nixpkgs; };
  gitlab-utils = import ./gitlab-utils;
  rel-eng = import ./rel-eng { inherit nixpkgs; };
in
  symlinkJoin {
    name = "ghc-utils";
    paths = [
      gdb.rr gdb.gdb gdb.run-ghc-gdb gdb.run-ghc-rr gdb.dot2svg 
      #misc-scripts rel-eng gitlab-utils
    ];
  }
