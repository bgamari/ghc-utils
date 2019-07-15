let
  np = import <nixpkgs> {};
  d = import ./default.nix {};
in
  np.mkShell { buildInputs = [d]; }
