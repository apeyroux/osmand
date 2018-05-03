(import <nixpkgs> {}).haskellPackages.developPackage { root = ./.; } # nix-build -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/master.tar.gz
# { pkgs ? import /home/alex/src/nixpkgs {} }:
# (import /home/alex/src/nixpkgs {}).haskell.packages.ghc841.developPackage { root = ./.; }
