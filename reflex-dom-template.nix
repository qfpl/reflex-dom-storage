{ mkDerivation, aeson, base, bytestring, containers, dependent-map
, dependent-sum, exception-transformers, ghcjs-dom, jsaddle, mmorph
, mtl, ref-tf, reflex, reflex-dom-core, reflex-dom-nested-routing
, stdenv, text
}:
mkDerivation {
  pname = "reflex-dom-storage";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers dependent-map dependent-sum
    exception-transformers ghcjs-dom jsaddle mmorph mtl ref-tf reflex
    reflex-dom-core reflex-dom-nested-routing text
  ];
  license = stdenv.lib.licenses.bsd3;
}
