{ nixpkgs ? (import <nixpkgs> {}) }:

with nixpkgs;
let
  fetch-gitlab-artifacts = nixpkgs.callPackage ./fetch-gitlab-artifacts {};

  bindistPrepEnv = pkgs.buildFHSUserEnv {
    name = "enter-fhs";
    targetPkgs = pkgs: with pkgs; [
      # all
      perl gcc binutils gnumake gmp ncurses5 git 
      # source-release.sh
      xlibs.lndir curl python3 which automake autoconf m4 file 
      ghc haskellPackages.happy haskellPackages.alex
    ];
    runScript = "$SHELL";
  };

  scripts = stdenv.mkDerivation {
    name = "rel-eng-scripts";
    nativeBuildInputs = [ makeWrapper ];
    preferLocalBuild = true;
    buildCommand = ''
      mkdir -p $out/bin

      makeWrapper ${./upload.sh} $out/bin/upload.sh \
        --prefix PATH : ${lzip}/bin \
        --prefix PATH : ${s3cmd}/bin \
        --prefix PATH : ${gnupg}/bin \
        --set ENTER_FHS_ENV ${bindistPrepEnv}/bin/enter-fhs \

      makeWrapper ${./bin-release.sh} $out/bin/bin-release.sh

      cat > $out/bin/source-release.sh <<EOF
      #!/bin/sh
      ${bindistPrepEnv}/bin/enter-fhs ${./source-release.sh}
      EOF
      chmod ugo+rx $out/bin/source-release.sh
    '';
  };

in
  symlinkJoin {
    name = "ghc-rel-eng";
    preferLocalBuild = true;
    paths = [ scripts fetch-gitlab-artifacts ];
  }
