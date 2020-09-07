{ compiler ? "ghc884"
, lib
, pkgs
}:

let
  inherit (pkgs.lib.trivial) flip pipe;
  inherit (pkgs.haskell.lib) appendPatch appendConfigureFlags;

  hakyllFlags = [ "-f" "watchServer" "-f" "previewServer" ];

  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hpNew: hpOld: {
      hakyll =
        pipe
           hpOld.hakyll
           [ (flip appendPatch ./hakyll.patch)
             (flip appendConfigureFlags hakyllFlags)
           ];

      robertwpearce-com = hpNew.callCabal2nix "robertwpearce-com" ./. { };
    };
  };
in haskellPackages
