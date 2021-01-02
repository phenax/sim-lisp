{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  haskellDeps = ps: with ps; [
    base
  ];

  ghc = haskellPackages.ghcWithPackages haskellDeps;

  nixPackages = [
    #pkgs.ghc
    haskellPackages.haskell-language-server
    haskellPackages.cabal-install
    pkgs.entr # Re-run on file change
  ];
in pkgs.stdenv.mkDerivation {
  name = "env";
  buildInputs = nixPackages;
}
