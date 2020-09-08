let
  sources = import ./nix/sources.nix;
in
  { pkgs ? import sources.nixpkgs { } }:

  let
    cfg = import ./nix/default.nix { };
  in pkgs.stdenv.mkDerivation {
    name = "robertwpearce-com-shell";
    buildInputs = cfg.tools;
    shellHook = ''
      ${cfg.ci.pre-commit-check.shellHook}
    '';
  }
