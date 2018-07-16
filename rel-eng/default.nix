{ nixpkgs ? (import <nixpkgs> {}) }:

with nixpkgs;
stdenv.mkDerivation {
  name = "rel-eng-scripts";
  nativeBuildInputs = [ makeWrapper ];
  buildCommand = ''
    mkdir -p $out/bin
    makeWrapper ${./upload.sh} $out/bin/upload.sh \
      --prefix PATH : ${s3cmd}/bin
    makeWrapper ${./bin-release.sh} $out/bin/bin-release.sh
  '';
}
