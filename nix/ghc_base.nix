{ compilerVersion ? "ghc8107"
}:

let
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/ff377a78794d412a35245e05428c8f95fef3951f.tar.gz") { };
  compiler = pkgs.haskell.packages."${compilerVersion}";
in
pkgs.mkShell {
  buildInputs =
    [
      pkgs.cabal-install
      compiler.ghc
    ];
}
