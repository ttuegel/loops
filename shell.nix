{ pkgs ? (import <nixpkgs> {}), haskellPackages ? pkgs.haskellPackages_ghc782
, hsDevTools ? pkgs.hsDevTools }:

let inherit (haskellPackages)
      cabal criterion primitive tasty tastyQuickcheck transformers vector;
in
cabal.mkDerivation (self: {
  pname = "free-loops";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [ primitive transformers vector ];
  testDepends = [ criterion tasty tastyQuickcheck ];
  buildTools = hsDevTools haskellPackages;
  meta = {
    description = "Fast imperative-style loops as a free monad EDSL";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
