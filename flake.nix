{
  description = "thegame-hs";

  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    haskell-nix,
    pre-commit-hooks,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      ghcVersion = "923";
      compiler-nix-name = "ghc" + ghcVersion;

      pre-commit-check =
        pre-commit-hooks.lib.${system}.run
        {
          src = ./.;

          settings = {
            ormolu.cabalDefaultExtensions = true;
          };

          hooks = {
            alejandra.enable = true;
            cabal-fmt.enable = true;
            fourmolu.enable = true;
            hlint.enable = true;
          };

          tools = {};
        };

      hls = pkgs.haskell-language-server.override {supportedGhcVersions = [ghcVersion];};

      overlays = [
        haskell-nix.overlay
        (final: prev: {
          thegame-hs = final.haskell-nix.project' {
            src = ./.;
            inherit compiler-nix-name;
            shell = {
              buildInputs = with pkgs; [
                alejandra
                hls
                haskellPackages.hlint
                cabal-install
              ];
              crossPlatform = [];
              inherit (pre-commit-check) shellHook;
            };
          };
        })
      ];
      pkgs = import nixpkgs {
        inherit system overlays;
        inherit (haskell-nix) config;
      };
      flake = pkgs.thegame-hs.flake {crossPlatforms = p: [];};
      defaultPackage = flake.packages."thegame-hs:exe:thegame-hs";
      devShells = {
        default = flake.devShell;
        hooks = pkgs.mkShell {
          inherit (pre-commit-check) shellHook;
        };
      };
    in
      flake
      // {
        inherit defaultPackage devShells;
        packages.docker-image = pkgs.dockerTools.buildImage {
          name = "thegame-hs";
          tag = "latest";
          contents = with pkgs; [
            package
            busybox
          ];

          config = {
            Cmd = ["/bin/thegame-hs"];
          };
        };

        checks = {
          format-check = pre-commit-check;
        };
      });
}
