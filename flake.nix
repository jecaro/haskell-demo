{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  outputs = { self, nixpkgs }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in
    {
      packages.x86_64-linux.default = pkgs.stdenv.mkDerivation {
        pname = "haskell-demo";
        version = "0.0.1";
        src = self;
        buildInputs = [ pkgs.haskellPackages.ghc ];
        buildPhase = ''
          ghc Main.hs -o haskell-demo
        '';
        installPhase = ''
          mkdir -p $out/bin
          cp haskell-demo $out/bin/
        '';

      };

      devShell.x86_64-linux =
        pkgs.mkShell {
          buildInputs = [
            pkgs.ghcid
            (pkgs.haskellPackages.ghcWithPackages (pkgs_: [
              pkgs_.cabal-install
              pkgs_.haskell-language-server
              pkgs_.hlint
              pkgs_.implicit-hie
            ]))
          ];
        };
    };
}
