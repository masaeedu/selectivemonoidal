let
  sources = import ./nix/sources.nix;
  compilerVersion = "ghc865";
  pkgs = (import sources.iohk-nixpkgs) (import sources.iohk-hnix);
in
pkgs.haskell-nix.cabalProject {
  src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
  ghc = pkgs.buildPackages.pkgs.haskell-nix.compiler.${compilerVersion};
}
