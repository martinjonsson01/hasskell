{
  description = "Hasskell is a Home Assistant framework in Haskell.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    stacklock2nix.url = "github:cdepillabout/stacklock2nix/v5.2.1";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    stacklock2nix,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [stacklock2nix.overlay];
      };

      haskellPackages = pkgs.haskell.packages.ghc9103;
      hlib = pkgs.haskell.lib;

      jailbreakUnbreak = pkg: hlib.doJailbreak (pkg.overrideAttrs (_: {meta = {};}));

      # Wrap Stack to work with our Nix integration. We do not want to modify
      # stack.yaml so non-Nix users do not notice anything.
      # - no-nix: We do not want Stack's way of integrating Nix.
      # --system-ghc    # Use the existing GHC on PATH (will come from this Nix file)
      # --no-install-ghc  # Do not try to install GHC if no matching GHC found on PATH
      stack-wrapped = pkgs.symlinkJoin {
        name = "stack"; # will be available as the usual `stack` in terminal
        paths = [pkgs.stack];
        buildInputs = [pkgs.makeWrapper];
        postBuild = ''
          wrapProgram "$out"/bin/stack \
            --add-flags "\
              --no-nix \
              --system-ghc \
              --no-install-ghc \
            "
        '';
      };

      hasskell-stacklock = pkgs.stacklock2nix {
        stackYaml = ./stack.yaml;
        all-cabal-hashes = pkgs.fetchFromGitHub {
          owner = "commercialhaskell";
          repo = "all-cabal-hashes";
          rev = "1b97f47fdd2848b37ee653a377582159b43de1d0";
          sha256 = "sha256-nI6lB1pnePu/yyxdPlAttI8d2++3qqdfAirxDgyJydA=";
        };
      };

      hasskell-pkg-set = haskellPackages.override (oldAttrs: {
        inherit (hasskell-stacklock) all-cabal-hashes;

        overrides = pkgs.lib.composeManyExtensions [
          (oldAttrs.overrides or (_: _: {}))

          hasskell-stacklock.stackYamlResolverOverlay
          hasskell-stacklock.stackYamlExtraDepsOverlay
          hasskell-stacklock.stackYamlLocalPkgsOverlay
          hasskell-stacklock.suggestedOverlay

          (hfinal: hprev: {
            # Tests currently don't pass in Nix, since it depends on an
            # external home assistant server to be running
            # (should be changed to use an internally spawned test server).
            hasskell-lib = hlib.dontCheck hprev.hasskell-lib;

            # Tests don't build due to QuickCheck 2.15 not working with ghc 9.10
            heap = hlib.dontCheck hprev.heap;
            selective = hlib.dontCheck hprev.selective;
            # Diagnose has an out of date upper bound on Text (<= 2.0)
            diagnose = jailbreakUnbreak hprev.diagnose;
            # Fails due to "`cc' failed in phase `Haskell C pre-processor'."
            path = hlib.dontCheck hprev.path;
            # Fails due to "ghc-9.10.3: Haskell pre-processor: could not execute: doctest-discover"
            doctest-discover = hlib.dontCheck hprev.doctest-discover;
            # Tests don't compile.
            xmlgen = hlib.dontCheck hprev.xmlgen;
            HTF = hlib.dontCheck hprev.HTF;
          })
        ];
      });
    in {
      packages.hasskell = hasskell-pkg-set.hasskell-cli;

      packages.default = self.packages.${system}.hasskell;

      devShells.default = hasskell-pkg-set.shellFor {
        packages = haskPkgs: hasskell-stacklock.localPkgsSelector haskPkgs;
        nativeBuildInputs = [
          haskellPackages.haskell-language-server
          haskellPackages.ghc
          haskellPackages.hoogle
          haskellPackages.ghcid
          stack-wrapped
          pkgs.zlib
        ];
      };
    });
}
