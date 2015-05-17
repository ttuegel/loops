{ mkDerivation, base, primitive, stdenv, tasty, tasty-quickcheck
, transformers, vector
}:
mkDerivation {
  pname = "loops";
  version = "0.3.0.0";
  src = ./.;
  buildDepends = [ base primitive transformers vector ];
  testDepends = [ base tasty tasty-quickcheck ];
  description = "Fast imperative-style loops";
  license = stdenv.lib.licenses.bsd3;
}
