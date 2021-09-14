{ nixpkgs ? import <nixpkgs> {}, haskellPackages ? nixpkgs.haskellPackages, compiler ? "default", doBenchmark ? false }:

let
  inherit (nixpkgs) pkgs;
  systemPackages = [
    haskellPackages.haskell-language-server
    haskellPackages.cabal-install
    pkgs.entr # Re-run on file change
  ];

  commonHsPackages = with haskellPackages; [
    base
    bytestring
    containers
    file-embed
    random
    parsec
    raw-strings-qq
    transformers
  ];
in
  with haskellPackages; mkDerivation {
    pname = "simlisp";
    version = "0.1.0.0";
    src = ./.;
    isLibrary = true;
    isExecutable = true;
    libraryHaskellDepends = commonHsPackages;
    executableHaskellDepends = commonHsPackages;
    executableSystemDepends = systemPackages;
    testHaskellDepends = commonHsPackages ++ [ hspec ];
    license = "MIT";
    hydraPlatforms = stdenv.lib.platforms.none;
  }
