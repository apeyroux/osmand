with import <nixpkgs> {};

let
  version = "1.3.0";
  drv = (haskellPackages.override {
   overrides = self: super: rec {
   };
  }).callCabal2nix "osmand" ./. {};
in if lib.inNixShell then drv.env.overrideAttrs (old: {
  buildInputs = old.buildInputs ++ [ haskellPackages.ghcid cabal-install ];
}) else drv
