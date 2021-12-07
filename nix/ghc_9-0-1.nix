{}:

let
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/8a308775674e178495767df90c419425474582a1.tar.gz") { };
  srcs = import ./sources.nix;
  compilerVersion = "ghc901";
  compiler = pkgs.haskell.packages."${compilerVersion}";
in
compiler.developPackage {
  name = "algebra-simple";
  root = ../.;
  returnShellEnv = true;
  modifier = drv:
    pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages; [
      cabal-install
      pkgs.zlib
    ]);
  overrides = final: prev: with pkgs.haskellPackages; {
    refined-simple =
      final.callCabal2nix "refined-simple" srcs.refined-simple-src { };
  };
}
