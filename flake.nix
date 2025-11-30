{
  description = "Hasskell is a Home Assistant framework in Haskell.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};

      haskellPackages = pkgs.haskell.packages.ghc9103;

      jailbreakUnbreak = pkg:
        pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: {meta = {};}));

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
    in {
      packages.hasskell = haskellPackages.callCabal2nix "hasskell" ./hasskell-cli {
        # Dependency overrides go here
        hasskell-lib = pkgs.haskell.lib.dontCheck ((haskellPackages.callCabal2nix "hasskell-lib" ./hasskell-lib {}).overrideAttrs (old: {
          buildInputs = with pkgs; old.buildInputs ++ [zlib];
        }));
      };

      packages.default = self.packages.${system}.hasskell;
      defaultPackage = self.packages.${system}.default;

      devShells.default = pkgs.mkShell rec {
        buildInputs = [
          haskellPackages.haskell-language-server
          haskellPackages.ghc
          haskellPackages.cabal-install
          haskellPackages.hoogle
          haskellPackages.ghcid
          stack-wrapped
          pkgs.zlib
        ];
        inputsFrom = map (__getAttr "env") (__attrValues self.packages.${system});

        NIX_CFLAGS_COMPILE = "-I${pkgs.zlib.dev}/include";
        NIX_LDFLAGS = "-L${pkgs.zlib.dev}/lib";
        LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath buildInputs;
      };
      devShell = self.devShells.${system}.default;
    });
}
