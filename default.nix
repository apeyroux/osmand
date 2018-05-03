# (import <nixpkgs> {}).haskellPackages.developPackage { root = ./.; }
# { pkgs ? import /home/alex/src/nixpkgs {} }:
(import /home/alex/src/nixpkgs {}).haskell.packages.ghc841.developPackage { root = ./.; }
