let
  sources = import ./nix/sources.nix;
  compilerVersion = "ghc865";
  ghcide = (import sources.ghcide-nix {})."ghcide-${compilerVersion}";
  pkgs = import sources.iohk-nixpkgs {};
  pkgs' = import sources.nixpkgs {};
  hspkgs = import ./default.nix;
in
hspkgs.shellFor {
  withHoogle = true;
  buildInputs = [
    ghcide
    pkgs.haskellPackages.ghcid
    pkgs'.cabal-install
    pkgs'.haskellPackages.hpack
  ];
}
