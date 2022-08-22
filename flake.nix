{
  description = "An alternative mathematical interface to Haskell's Num.";
  inputs = {
    # NIX
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
    #nixpkgs.url = "github:nixos/nixpkgs?rev=eaf2e7ebdef986ea29421882407cc93bec018b9a";
    nixpkgs.url = "github:nixos/nixpkgs?rev=44a97867600d7148298e363f34cfcaee9ba486c4";
    #nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    # LIQUID

    # TODO: Without an override, nix uses the liquid-fixpoint found in nixpkgs.
    # This may not be the right hash since we are overriding liquidhaskell
    # below, and liquid-fixpoint is a submodule of liquidhaskell. Thus we need
    # to use whatever hash is referenced in the ucsd-progsys/liquidhaskell
    # repo.
    #
    # It would be nice if we could automatically reference the hash rather than
    # directly copy it here. Then when we update liquidhaskell we would not also
    # have to find and update the liquid-fixpoint hash as well.
    #
    # Try: importing LH below w/ ?submodules=1. investigate in repl
    # or submodule = true
    liquid-fixpoint-src = {
      url = "github:ucsd-progsys/liquid-fixpoint?rev=f6af0464c0b2ccd908075d5254864dbcf73f8e7a";
      flake = false;
    };

    # Overriding LH because the version in nixpkgs does not build w/ the
    # nixpkgs version of rest-rewrite. Unfortunately, the alternative
    # strategy of "downgrade rest-rewrite" is difficult work with our
    # desired ghc (9) because it requires an old hashable dep that we
    # cannot jailbreak. Using the more up-to-date LH deps seems like the
    # better strategy for now...
    liquidhaskell-src.url = "github:ucsd-progsys/liquidhaskell/develop";
  };
  outputs =
    { flake-compat
    , flake-utils
    , liquid-fixpoint-src
    , liquidhaskell-src
    , nixpkgs
    , self
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        inherit system;
        config = {
          # for rest-rewrite in nixpkgs
          allowBroken = true;
        };
      };
      buildTools = c: with c; [
        cabal-install
        pkgs.gnumake
        pkgs.zlib
      ];
      liquidTools = with pkgs; [
        z3
      ];
      devTools = c: with c; [
        ghcid
        haskell-language-server
      ];

      overrideHackageIndex = hp: hp.override {
        all-cabal-hashes = builtins.fetchurl {
          url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/84a454377bc82a2c90a91790216699f67b266a95.tar.gz";
          sha256 = "06kdslsyh7fn63zrgi02isvn9fc2j3g9a2j7x32mjpb2rml65p56";
        };
      };
      hlib = pkgs.haskell.lib;
      withZ3 = p: hlib.overrideCabal p (old: {
        buildTools = (old.buildTools or [ ]) ++ [ pkgs.z3 ];
      });
      noProf = p: hlib.overrideCabal p (old: {
        enableLibraryProfiling = false;
      });

      ghc-version = "ghc901";
      compiler = pkgs.haskell.packages."${ghc-version}";
      mkPkg = returnShellEnv: withDevTools:
        compiler.developPackage {
          inherit returnShellEnv;
          name = "algebra-simple";
          root = ./.;
          modifier = drv:
            pkgs.haskell.lib.addBuildTools drv
              (buildTools compiler ++ liquidTools ++
                (if withDevTools then devTools compiler else [ ]));
          overrides = final: prev: with compiler; {
            # LIQUID
            liquid-base = noProf (withZ3 prev.liquid-base);

            liquid-ghc-prim = withZ3 prev.liquid-ghc-prim;

            # the liquid-fixpoint on hackage is not compatible with its loose
            # rest-rewrite bound, hence using the later version from github,
            # which is fixed.
            liquid-fixpoint =
              hlib.doJailbreak (final.callCabal2nix "liquid-fixpoint" liquid-fixpoint-src { });

            liquidhaskell =
              hlib.dontCheck
                (final.callCabal2nix "liquidhaskell" liquidhaskell-src { });

            rest-rewrite =
              hlib.dontCheck
                ((overrideHackageIndex compiler).callHackage "rest-rewrite" "0.4.0" { });

            # OTHERS

            env-guard =
              (overrideHackageIndex compiler).callHackage "env-guard" "0.2"
                { };

            lens-family = prev.lens-family_2_1_1;
            lens-family-core = prev.lens-family-core_2_1_0;

            optics = prev.optics_0_4;
            optics-core = prev.optics-core_0_4;
            optics-extra = prev.optics-extra_0_4;
            optics-th = prev.optics-th_0_4;
            optparse-applicative = prev.optparse-applicative_0_15_1_0;
            recursion-schemes = hlib.dontCheck (final.callHackage "recursion-schemes" "5.2.1" { });
          };
        };
    in
    {
      packages.default = mkPkg false false;

      devShells.default = mkPkg true true;
      devShells.ci = mkPkg true false;
    });
}
