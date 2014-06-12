{ cabal, primitive, tasty, tastyQuickcheck, transformers, vector }:

cabal.mkDerivation (self: {
  pname = "loops";
  version = "0.2.0.0";
  #sha256 = "0kygwz7rdnnk6l97g63ac69321raqhdr6hbq014ffqmmp9s0xxg2";
  src = ./.;
  buildDepends = [ primitive transformers vector ];
  testDepends = [ tasty tastyQuickcheck ];
  hyperlinkSource = true;
  meta = {
    description = "Fast imperative-style loops";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
