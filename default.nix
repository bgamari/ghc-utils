with (import <nixpkgs> {});

let
  rel-eng =
    stdenv.mkDerivation {
      name = "rel-eng-scripts";
      buildCommand = ''
        mkdir -p $out/bin

        cp ${./debug-ghc} $out/bin/debug-ghc
        chmod ugo+rx $out/bin/debug-ghc
        substituteInPlace $out/bin/debug-ghc \
          --replace tempfile ${debianutils}/bin/tempfile \
          --replace 'PROG="gdb' 'PROG="${gdb.gdb}/bin/gdb'
      '';
    };
  gdb = import ./gdb;
  gitlab-utils = import ./gitlab-utils;
in
  symlinkJoin {
    name = "hi";
    paths = [ gdb.gdb gdb.run-ghc-gdb gdb.run-ghc-rr gdb.dot2svg rel-eng gitlab-utils ];
  }
