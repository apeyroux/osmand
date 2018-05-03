{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  myPython = python.withPackages (ps: with ps; [
  lxml
  click
  clint
  requests
  virtualenv ]);
in {
  env = stdenv.mkDerivation {
    name = "osmand-dwl";
    buildInputs = [
      myPython
      gcc
      libxml2
      libzip ];
  };
}
