{ pkgs ? (import <nixpkgs> {}), haskellPackages ? pkgs.haskellPackages_ghc782
, hsDevTools ? pkgs.hsDevTools }:

let inherit (haskellPackages)
      cabal bifunctors free strict tasty tastyQuickcheck transformers;
in
cabal.mkDerivation (self: {
  pname = "free-loops";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [ bifunctors free strict transformers ];
  testDepends = [ strict tasty tastyQuickcheck ];
  buildTools = hsDevTools haskellPackages;
  meta = {
    description = "Fast imperative-style loops as a free monad EDSL";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
