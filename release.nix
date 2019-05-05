with import <nixpkgs> {};

let

  osmand = import ./default.nix;

  static-pkgs = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/88ae8f7d.tar.gz) {};

  alpine = dockerTools.pullImage {
    imageName = "alpine";
    imageDigest = "sha256:7df6db5aa61ae9480f52f0b3a06a140ab98d427f86d8d5de0bedab9b8df6b1c0";
    sha256 = "05wcg38vsygjzf59cspfbb7cq98c7x18kz2yym6rbdgx960a0kyq";
  };

  vmDebian = (vmTools.diskImageFuns.debian9x86_64 {
    packagesList = fetchurl {
        url = mirror://debian/dists/jessie/main/binary-amd64/Packages.xz;
        sha256 = "09y1mv4kqllhxpk1ibjsyl5jig5bp0qxw6pp4sn56rglrpygmn5x";
    };
});

in rec {

  docker = dockerTools.buildImage {
    fromImage = alpine;
    name = "osmand";
    tag = osmand.version;
    created = "now";
    contents = [
      osmand
    ];
    config = {
      Version = "1.3";
      EntryPoint = ["osmand"];
    };
  };

  static-bin = static-pkgs.pkgsMusl.callPackage ((fetchTarball https://github.com/apeyroux/static-haskell-nix/archive/apeyroux.tar.gz) + "/survey") {
  # static-bin = static-pkgs.pkgsMusl.callPackage (/home/alex/src/static-haskell-nix/survey) {
    normalPkgs = static-pkgs;
  };

  src-tar = releaseTools.sourceTarball {
    version = "1.3";
    buildInputs = [ cabal-install ];
    distPhase = ''
    cabal sdist
    mkdir -p $out/tarballs/
    cp dist/${osmand.name}.tar.gz $out/tarballs/
    '';
    src = ./.;
  };

  bin-tar = releaseTools.binaryTarball {
    doCheck = false;
    showBuildStats = false;
    buildPhase = "";
    installPhase = ''
      releaseName=${osmand.name}
      ${coreutils}/bin/install --target-directory "$TMPDIR/inst/bin" -D ${static-bin.haskellPackages.osmand}/bin/osmand
      md5sum ${static-bin.haskellPackages.osmand}/bin/osmand > $TMPDIR/inst/osmand.md5
    '';
    src = ./.;
  };

  osmand-fhs = (buildFHSUserEnv {
																	name = "osmand-fhs";
																	targetPkgs = (p: [ p.cabal-install p.ghc p.binutils.bintools p.zlib.dev ]);
																}).env;


  livraison-stig = stdenv.mkDerivation {
    name = "osmand-livraison-stig";
    src = ./.;
    buildPhase = ''
    now=$(date +"%d%m%Y-%H%M")
    mkdir osmand-$now
    ${coreutils}/bin/install --target-directory "osmand-$now/" -D ${static-bin.haskellPackages.osmand}/bin/osmand
    md5sum osmand-$now/osmand > osmand-$now/osmand.md5
    tar -zcvf osmand-$now.tar.gz osmand-$now
    '';
    installPhase = ''
    mkdir $out
    cp osmand-$now.tar.gz $out/
    md5sum osmand-$now.tar.gz > $out/osmand-$now.tar.gz.md5
    echo "
========================== MAIL STIG ==========================

Bonjour, 

Vous trouverez ci-joint la dernière livraison du binaire osmand :

$(md5sum osmand-$now.tar.gz)
$(cat osmand-$now/osmand.md5)

Cordialement

========================== MAIL STIG ========================== 
    "
    '';
  };

	deb = releaseTools.debBuild {
		name = osmand.name;
    version = osmand.version;
    diskImage = vmDebian;
    extraPackages = ["libgmp-dev" "libffi-dev"];
    debMaintainer = "Alexandre Peyroux <alex@px.io>";
    debRequires = ["libgmp-dev"
                   "libffi-dev"];
    doCheck = false;
    doInstallCheck = false;
    showBuildStats = false;
    preInstall = ''
echo ${vmDebian}
curl google.fr
cat <<EOT >> Makefile
install:
	tar -jxvf ${bin-tar}/tarballs/${osmand.name}.tar.bz2
	cp ./bin/osmand /usr/local/bin/
EOT
    '';
    src = src-tar;
  };
}
