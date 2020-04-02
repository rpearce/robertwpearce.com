{ }:

let
  sources = import ./nix/sources.nix;
  pkgs = (import sources.nixpkgs) { config = { allowUnfree = true; }; };
  haskellPackages = pkgs.haskellPackages;
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
      ];

      withHoogle = true;
    };
  }
