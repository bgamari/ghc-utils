# Usage:  nix build -f ./. env
#
# Start `result/bin/gdb` and run `source result/gdbinit` once a Haskell executable is loaded.

let
  rev = "08d245eb31a3de0ad73719372190ce84c1bf3aee";
  baseNixpkgs =
    builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = "1g22f8r3l03753s67faja1r0dq0w88723kkfagskzg9xy3qs8yw8";
  };
  nixpkgs = import baseNixpkgs {};

in with nixpkgs; rec {
  pythonPackages = python3Packages;

  ghc-gdb = pythonPackages.buildPythonPackage {
    name = "ghc-gdb";
    doCheck = false;
    src = ./.;
  };

  run-ghc-gdb = writeScriptBin "ghc-gdb" ''
    ${gdb}/bin/gdb -x ${gdbinit}/gdbinit
  '';

  run-ghc-rr = writeScriptBin "ghc-rr" ''
    args="$@"
    if [[ "$1" == "replay" ]]; then
      args="$args --gdb ${gdb}/bin/gdb -x ${gdbinit}/gdbinit"
    fi
    ${rr}/bin/rr $args
  '';

  gdb = nixpkgs.gdb.override {
    python = python3;
  };

  rr = nixpkgs.rr.overrideAttrs (oldAttrs: {
    src = fetchFromGitHub {
      owner = "mozilla";
      repo = "rr";
      rev = "c76af22201985d53998c9d4fac9d29a59e2dcf1b";
      sha256 = "1kc9vzij42a42mhd9fr7py122brcy5qmm557drapvmkh75jny7ff";
    };
  });

  pythonEnv = python3.withPackages (_: [ ghc-gdb ]);

  env = symlinkJoin {
    name = "gdb-with-ghc-gdb";
    paths = [ gdb pythonEnv gdbinit rr dot2svg ];
  };

  # useful to render `ghc closure-deps` output
  dot2svg = writeScriptBin "dot2svg" ''
    if [[ $# == 0 ]]; then
      echo "Usage: $0 [dot file]"
      exit 1
    fi
    ${graphviz}/bin/dot -T svg -o $1.svg $1
  '';

  gdbinit = writeTextFile {
    name = "gdbinit";
    destination = "/gdbinit";
    text = ''
      python sys.path = ["${pythonEnv}/lib/python3.6/site-packages"] + sys.path
      python
      try:
          import importlib
          importlib.reload(ghc_gdb)
      except NameError:
          import ghc_gdb
      end

      echo The `ghc` command is now available.\n
    '';
  };
}

