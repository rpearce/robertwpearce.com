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
  LANG = "en_US.UTF-8";
  buildPhase = ''
    hakyll-site --verbose build
  '';
  installPhase = ''
    mkdir -p "$out"
    cp -r ../dist/* "$out"
  '';
}
#npm install --prefix js
#npm run build --prefix js
