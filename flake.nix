{
  description = "algebra-simple flake";
  inputs.nixpkgs.url = "github:nixos/nixpkgs?rev=98000933d72a97632caf0db0027ea3eb2e5e7f29";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs =
    { flake-utils
    , nixpkgs
    , self
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      compilerVersion = "ghc922";
      compiler = pkgs.haskell.packages."${compilerVersion}";
      mkPkg = returnShellEnv:
        compiler.developPackage {
          inherit returnShellEnv;
          name = "algebra-simple";
          root = ./.;
          modifier = drv:
            pkgs.haskell.lib.addBuildTools drv (with compiler; [
              cabal-install
              haskell-language-server
              ghcid
              ormolu
              pkgs.nixpkgs-fmt
              pkgs.zlib
            ]);
        };
    in
    {
      defaultPackage = mkPkg false;

      devShell = mkPkg true;
    });
}
