{ compiler ? "ghc865"
, pkgs ? import ./pkgs.nix
}:

let
  haskellPackages = pkgs.haskell.packages.${compiler};
  project = haskellPackages.callPackage ./project.nix { };
in
  {
    project = project;

    shell = haskellPackages.shellFor {
      packages = p: with p; [
        project
      ];
      buildInputs = with haskellPackages; [
        cabal-install
        ghcid
      ];
      withHoogle = true;
    };
  }
