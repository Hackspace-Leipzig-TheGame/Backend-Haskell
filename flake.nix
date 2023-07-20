{
  description = "the game hs";

  nixConfig.extra-trusted-public-keys = ["the-game-hs.cachix.org-1:8xuU4po51RvcZEoAbldcMh128URPKPW+h/hifFJoip0="];
  nixConfig.extra-substituters = ["the-game-hs.cachix.org"];

  inputs.nixpkgs.url = "github:nixos/nixpkgs";
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
      ghcVer = "96";
      server = hls.packages.${system}."haskell-language-server-${ghcVer}";
      compiler-nix-name = "ghc962";
      src = ./.;
      hspkgs = pkgs.haskell.packages.${compiler-nix-name}.override {
        overrides = hself: hsuper:
          with pkgs.haskell.lib.compose; {
            ormolu = hsuper.ormolu_0_7_1_0;
            the-game-hs = hself.callCabal2nix "the-game-hs" src {};
            foundation = overrideCabal (_drv: {patches = [];}) hsuper.foundation;
            warp = dontCheck hsuper.warp_3_3_27; # old warp doesn't compile
            recv = hsuper.recv_0_1_0; # warps needs newer recv
            crypton-x509 = dontCheck (markUnbroken hsuper.crypton-x509); # warp needs crypton but tests don't pass
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
          # hspkgs.haskell-language-server
          server
        ];
      };
    in {
      devShells.default = hsShell;
      packages.default = hspkgs.the-game-hs;
      checks.formatting = pcc;
    });
}
