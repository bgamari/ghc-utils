with (import <nixpkgs> {});

stdenv.mkDerivation {
  name = "rel-eng-scripts";
  buildCommand = ''
    mkdir -p $out/bin

    cp ${./debug-ghc} $out/bin/debug-ghc
    chmod ugo+rx $out/bin/debug-ghc
    substituteInPlace $out/bin/debug-ghc \
      --replace tempfile ${debianutils}/bin/tempfile
  '';
}
