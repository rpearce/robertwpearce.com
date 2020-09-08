let
  sources = import ./sources.nix;
in
  { pkgs ? import sources.nixpkgs { } }:

  let
    pre-commit-hooks = import sources."pre-commit-hooks.nix";
    gitignore = import sources."gitignore.nix" { inherit (pkgs) lib; };
    haskellPackages = pkgs.callPackage ../generator/hpkgs.nix { };
    generator = haskellPackages.callPackage ../generator/default.nix { };
  in {
    generator = generator;

    tools = [
      generator
      haskellPackages.ghcid
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
        src = gitignore.gitignoreSource ../src;
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
