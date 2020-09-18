let
  cfg = import ../nix/default.nix {};
  hp = cfg.haskellPackages;
in
{}:

  hp.shellFor {
    packages = p: [
      p.robertwpearce-com
    ];

    buildInputs = with hp; [
      cabal-install
      ghcid
      hlint
      hp.robertwpearce-com
      ormolu
    ];

    withHoogle = true;
  }
