{ compiler ? "ghc883"
, sources ? import ./nix/sources.nix
}:

let
  overlay = _: pkgz: {
    niv = import sources.niv { };
  };

  pkgs = import sources.nixpkgs {
    config = {
      packageOverrides = pkgz: rec {
        haskellPackages = pkgz.haskellPackages.override {
          overrides = hpNew: hpOld: rec {
            hakyll = hpOld.hakyll.overrideAttrs(oldAttrs: {
              configureFlags = "-f watchServer -f previewServer";
              patches = [ ./hakyll.patch ];
            });
            project = hpNew.callPackage ./project.nix { };
            #project = hpNew.callCabal2nix "robertwpearce-com" ./. { };
          };
        };
      };
    };
    overlays = [ overlay ];
  };

  haskellPackages = pkgs.haskell.packages.${compiler};
in
  {
    project = pkgs.haskellPackages.project;

    shell = haskellPackages.shellFor {
      packages = p: with p; [
        pkgs.haskellPackages.project
      ];
      buildInputs = with haskellPackages; [
        ghcid
        hlint
        niv
        #ormolu
        pkgs.cacert
        pkgs.nix
      ];
      withHoogle = true;
    };
  }
