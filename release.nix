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
        haskell = pkgz.haskell // {
          packages = pkgz.haskell.packages // {
            ${compiler} = pkgz.haskell.packages.${compiler}.override {
              overrides = hpNew: hpOld: rec {
                hakyll = hpOld.hakyll.overrideAttrs(oldAttrs: {
                  configureFlags = "-f watchServer -f previewServer";
                  patches = [ ./hakyll.patch ];
                });

                project = hpNew.callCabal2nix "robertwpearce-com" ./. { };
              };
            };
          };
        };
      };
    };

    overlays = [ overlay ];
  };

  haskellPackages = pkgs.haskell.packages.${compiler};

in {
  project = haskellPackages.project;

  shell = haskellPackages.shellFor {
    packages = p: with p; [
      haskellPackages.project
    ];
    buildInputs = with haskellPackages; [
      ghcid
      hlint # or ormolu
      niv
      pkgs.cacert
      pkgs.nix
    ];
    withHoogle = true;
  };
}
