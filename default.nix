let
  sources = import ./nix/sources.nix;
in
  { pkgs ? import sources.nixpkgs { } }:

  let
    generator = (import ./nix/default.nix { }).generator;
  in pkgs.stdenv.mkDerivation {
    name = "robertwpearce-com-site";
    buildInputs = [
      generator
    ];
    src = ./src;

    # https://github.com/jaspervdj/hakyll/issues/614
    # https://github.com/NixOS/nix/issues/318#issuecomment-52986702
    # https://github.com/MaxDaten/brutal-recipes/blob/source/default.nix#L24
    LOCALE_ARCHIVE = pkgs.lib.optionalString (pkgs.buildPlatform.libc == "glibc") "${pkgs.glibcLocales}/lib/locale/locale-archive";
    LANG = "en_US.UTF-8";

    buildPhase = ''
      hakyll-site --verbose build
    '';
    installPhase = ''
      mkdir -p "$out"
      cp -r ../dist/* "$out"
    '';
  }
