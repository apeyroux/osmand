with import <nixpkgs> {};

let

  python = import ./requirements.nix { inherit pkgs; };
  osmand-mirror = import ./default.nix {};

in {

  osmandMirrorAppImage = dockerTools.buildImage {
    name = "osmand-mirror";
    contents = [
      osmand-mirror
      glibc.bin
      glibcLocales
    ];
    config = {
      EntryPoint = ["osmand-mirror"];
      # for click
      Env = ["LOCALE_ARCHIVE=/lib/locale/locale-archive"
             "LANG=fr_FR.utf8"
             "LC_ALL=fr_FR.utf8"];
    };
  };
  
}
