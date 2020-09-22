let
  sources = import ./sources.nix;
  config = { allowBroken = true; };
in
{ pkgs ? import sources.nixpkgs { inherit config; } }:

  let
    pre-commit-hooks = import sources."pre-commit-hooks.nix";
    haskellPackages = pkgs.callPackage ../generator/hpkgs.nix {};
    generator = haskellPackages.callPackage ../generator/default.nix {};
    src = ../src;
  in
    {
      inherit generator haskellPackages pkgs src;

      tools = [
        generator
        pkgs.cacert
        pkgs.niv
        pkgs.nix
        pkgs.pre-commit
        pre-commit-hooks.hlint
        pre-commit-hooks.nixpkgs-fmt
        pre-commit-hooks.ormolu
      ];

      ci = {
        pre-commit-check = pre-commit-hooks.run {
          inherit src;

          hooks = {
            nix-linter.enable = true;
            nixpkgs-fmt.enable = true;
            ormolu.enable = true;
            shellcheck.enable = true;
          };
          excludes = [ "^nix/sources\.nix$" ];
        };
      };
    }
