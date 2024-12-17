{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
  outputs = { self, nixpkgs }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in
    {
      devShell.x86_64-linux =
        pkgs.mkShell {
          buildInputs = [
            pkgs.ghcid
            (pkgs.haskellPackages.ghcWithPackages (pkgs_: with pkgs_;
            [
              cabal-install
              haskell-language-server
              hlint
              implicit-hie
            ]))
          ];
        };
    };
}
