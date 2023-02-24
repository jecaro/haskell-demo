{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-22.11";
  outputs = { self, nixpkgs }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in
    {
      devShell.x86_64-linux =
        pkgs.mkShell {
          buildInputs = [
            pkgs.ghcid
            pkgs.haskellPackages.cabal-install
            pkgs.haskellPackages.haskell-language-server
            pkgs.haskellPackages.hlint
            pkgs.haskellPackages.implicit-hie
          ];
          # Change the prompt to show that you are in a devShell
          shellHook = "export PS1='\\[\\e[1;34m\\]dev > \\[\\e[0m\\]'";
        };
    };
}
