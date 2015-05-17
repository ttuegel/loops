{ mkDerivation, base, criterion, stdenv, vector }:
mkDerivation {
  pname = "loops";
  version = "0.3.0.0";
  src = ./.;
  buildDepends = [ base ];
  testDepends = [ base criterion vector ];
  description = "Fast imperative-style loops";
  license = stdenv.lib.licenses.bsd3;
}
