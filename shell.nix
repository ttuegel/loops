{ pkgs ? (import <nixpkgs> {})
, haskellPackages ? pkgs.haskellPackages
, hsDevTools ? pkgs.hsDevTools or null
}:

haskellPackages.callPackage ./default.nix {}