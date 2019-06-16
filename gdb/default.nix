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
    src = ./.;
    preferLocalBuild = true;
    checkInputs = [ mypy ];
    checkPhase =
      ''
        mypy --ignore-missing-imports .
      '';
  };

  run-ghc-gdb = writeScriptBin "ghc-gdb" ''
    ${gdb}/bin/gdb -x ${gdbinit}/gdbinit "$@"
  '';

  run-ghc-rr = writeScriptBin "ghc-rr" ''
    args="$@"
    if [[ "$1" == "replay" ]]; then
      args="$args --debugger ${gdb}/bin/gdb -x ${gdbinit}/gdbinit"
    fi
    ${rr}/bin/rr $args
  '';

  libipt = stdenv.mkDerivation {
    name = "libipt";
    nativeBuildInputs = [ cmake ];
    src = fetchFromGitHub {
      owner = "01org";
      repo = "processor-trace";
      rev = "f7a4ee88b32a0c2d148dd08268b94c4076da43d9";
      sha256 = "18wc5gk6vby3wr0rx9dgg841zb8yhsw1dbpvs6b1xs8c7j4q08mr";
    };
    #sourceRoot = "source/libipt";
  };

  gdb = (nixpkgs.gdb.override {
    python = python3;
  }).overrideAttrs (oldAttrs: {
    buildInputs = oldAttrs.buildInputs ++ [ libipt ];
  });

  rr = nixpkgs.rr.overrideAttrs (oldAttrs: {
    src = fetchFromGitHub {
      owner = "mozilla";
      repo = "rr";
      rev = "4b6a1edd321e057c4ae4c34e160bc5b7aa735c2d";
      sha256 = "1jhadi07kl6qd44f55ndcmv1w7v0s4bp4h0y4svj5plaa0y9cawx";
    };
  });

  pythonEnv = python3.withPackages (_: [ ghc-gdb ]);

  env = symlinkJoin {
    name = "gdb-with-ghc-gdb";
    paths = [
      gdb pythonEnv gdbinit rr dot2svg
      run-ghc-gdb run-ghc-rr
    ];
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
      if 'ghc_gdb' in globals():
          import importlib
          importlib.reload(ghc_gdb)
      else:
          try:
              import ghc_gdb
          except Exception as e:
              import textwrap
              print('Failed to load ghc_gdb:')
              print('  ', e)
              print("")
              print(textwrap.dedent("""
                If the failure is due to a missing symbol or type try
                running `import ghc_gdb` after running the inferior.
                This will load debug information that is lazily
                loaded.
              """))
      end

      echo The `ghc` command is now available.\n
    '';
  };
}

