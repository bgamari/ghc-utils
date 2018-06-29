{ nixpkgs ? (import <nixpkgs> {}) }:

with nixpkgs;
let
  bindistPrepEnv = pkgs.buildFHSUserEnv {
    name = "enter-fhs";
    targetPkgs = pkgs: with pkgs; [
      gnupg m4 file gdb git zlib pkgconfig perl rsync gcc binutils gnumake gmp ncurses5 openssh curl
    ];
    runScript = "$SHELL";
  };
in
  bindistPrepEnv
