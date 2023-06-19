{
  description = "the game hs";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  inputs.utils.url = "github:numtide/flake-utils";
  inputs.hls.url = "github:haskell/haskell-language-server";
  inputs.pchs.url = "github:cachix/pre-commit-hooks.nix";
  inputs.pchs.inputs.nixpkgs.follows = "nixpkgs";

  outputs = {
    nixpkgs,
    utils,
    hls,
    pchs,
    ...
  }:
    utils.lib.eachSystem ["x86_64-linux"] (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      ghcVer = "94";
      server = hls.packages.${system}."haskell-language-server-${ghcVer}";
      compiler-nix-name = "ghc${ghcVer}";
      src = ./.;
      hspkgs = pkgs.haskell.packages.${compiler-nix-name}.override {
        overrides = hself: _hsuper:
          with pkgs.haskell.lib.compose; {
            the-game-hs = hself.callCabal2nix "the-game-hs" src {};
          };
      };
      pcc = pchs.lib.${system}.run {
        inherit src;
        hooks = {
          alejandra.enable = true;
          deadnix.enable = true;
          statix.enable = true;
          fourmolu.enable = true;
          cabal-fmt.enable = true;
          hlint.enable = true;
        };
      };
      hsShell = hspkgs.shellFor {
        shellHook = ''
          ${pcc.shellHook}
          echo "Welcome to the devShell of the game hs"
        '';
        packages = p: [p.the-game-hs];
        nativeBuildInputs = [
          pkgs.haskellPackages.cabal-install
          pkgs.haskellPackages.hlint
          server
        ];
      };
    in {
      devShells.default = hsShell;
      packages.default = hspkgs.the-game-hs;
      checks.formatting = pcc;
    });
}
