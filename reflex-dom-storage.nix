{ mkDerivation, aeson, base, bytestring, containers, dependent-map
, dependent-sum, ghcjs-dom, jsaddle, mtl, ref-tf, reflex
, reflex-dom-core, stdenv, text
}:
mkDerivation {
  pname = "reflex-dom-storage";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers dependent-map dependent-sum
    ghcjs-dom jsaddle mtl ref-tf reflex reflex-dom-core text
  ];
  license = stdenv.lib.licenses.bsd3;
}
