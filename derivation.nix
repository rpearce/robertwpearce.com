{ lib
, pkgs
}:

let
  generator = pkgs.haskellPackages.callPackage ./generator/default.nix { };
in pkgs.stdenv.mkDerivation {
  name = "robertwpearce-com-site";
  buildInputs = [
    generator
    pkgs.nodejs-14_x
  ];
  src = ./site;
  buildPhase = ''
    hakyll-site --verbose build
  '';
  installPhase = ''
    mkdir -p "$out"
    cp -r ../site-dist/* "$out"
  '';
}
#npm install --prefix js
#npm run build --prefix js
