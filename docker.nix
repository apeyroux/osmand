with import <nixpkgs> {};

let
  osmand = import ./default.nix;
  alpine = dockerTools.pullImage {
    imageName = "alpine";
    imageDigest = "sha256:7df6db5aa61ae9480f52f0b3a06a140ab98d427f86d8d5de0bedab9b8df6b1c0";
    sha256 = "05wcg38vsygjzf59cspfbb7cq98c7x18kz2yym6rbdgx960a0kyq";
  };
in {
  osmandMirrorAppImage = dockerTools.buildImage {
    fromImage = alpine;
    name = "osmand";
    contents = [
      osmand
    ];
    config = {
      Version = "1.0";
      EntryPoint = ["osmand"];
    };
  };
}
