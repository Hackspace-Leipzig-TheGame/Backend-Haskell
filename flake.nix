{
  description = "the game hs";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  inputs.utils.url = "github:numtide/flake-utils";
  inputs.hls.url = "github:haskell/haskell-language-server";

  outputs = { self, nixpkgs, utils, hls}: utils.lib.eachSystem ["x86_64-linux"] (system: let
    pkgs = nixpkgs.legacyPackages.${system};
    ghcVer = "94";
    server = hls.packages.${system}."haskell-language-server-${ghcVer}";
    compiler-nix-name = "ghc${ghcVer}";
    hspkgs = pkgs.haskell.packages.${compiler-nix-name}.override {
      overrides = hself: hsuper: with pkgs.haskell.lib.compose; {
        the-game-hs = hself.callCabal2nix "the-game-hs" ./. {};
      };
    };
    hsShell = hspkgs.shellFor {
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
  });
}
