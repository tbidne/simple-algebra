{ compiler ? "ghc8107"
, pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/2deb07f3ac4eeb5de1c12c4ba2911a2eb1f6ed61.tar.gz") { }
}:

let
  haskellDeps = ps: with ps; [
    cabal-install
    cabal-fmt
    hlint
  ];

  haskellOtherDeps = [ pkgs.haskellPackages.ormolu ];

  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages haskellDeps;

  otherDeps = with pkgs; [
    nixpkgs-fmt
    zlib
  ];
in
pkgs.mkShell {
  buildInputs =
    [ ghc ]
    ++ haskellOtherDeps
    ++ otherDeps;
}
