{ lib, pkgs }:

let
  generator = pkgs.haskellPackages.callPackage ./generator/default.nix { };
in pkgs.stdenv.mkDerivation {
  name = "robertwpearce-com-site";
  buildInputs = [
    generator
    pkgs.nodejs-14_x
  ];
  src = ./src;
  LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
  LANG = "en_US.UTF-8";
  buildPhase = ''
    hakyll-site --verbose build
  '';
  installPhase = ''
    mkdir -p "$out"
    cp -r ../dist/* "$out"
  '';
}
