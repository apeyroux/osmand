{ }:

let
  pkgs = import <nixpkgs> {};
  python = import ./requirements.nix { inherit pkgs; };
in python.mkDerivation {
  name = "osmand-mirror";
  src = ./.;
  buildInputs = [
    python.packages."lxml"
    python.packages."clint"
    python.packages."requests"
    python.packages."click"
  ];
  propagatedBuildInputs = [
    python.packages."lxml"
    python.packages."clint"
    python.packages."requests"
    python.packages."click"
  ];
}
