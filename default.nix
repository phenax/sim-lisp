{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  nixPackages = [
    haskellPackages.haskell-language-server
    haskellPackages.cabal-install
    pkgs.entr # Re-run on file change
  ];
in pkgs.stdenv.mkDerivation {
  name = "env";
  buildInputs = nixPackages;
}
