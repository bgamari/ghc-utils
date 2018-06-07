{ mkDerivation, base, containers, optparse-applicative, stdenv
, text, trifecta
}:
mkDerivation {
  pname = "compare-ticks";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers optparse-applicative text trifecta
  ];
  homepage = "http://github.com/bgamari/ghc-utils";
  description = "Compare ticky profiles";
  license = stdenv.lib.licenses.bsd3;
}
