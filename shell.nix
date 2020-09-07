let
  sources = import ./nix/sources.nix;
in
  { pkgs ? import sources.nixpkgs { } }:

  let
    haskellPackages = pkgs.callPackage ./generator/hpkgs.nix { };
    generator = haskellPackages.callPackage ./generator/default.nix { };
  in pkgs.stdenv.mkDerivation {
    name = "env";
    buildInputs = with pkgs; [
      generator
      haskellPackages.ghcid
      haskellPackages.ormolu
      niv
      cacert
      nix
    ];
  }
