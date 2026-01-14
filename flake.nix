{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  outputs = { self, nixpkgs }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in
    {
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
