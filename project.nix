{ mkDerivation, base, hakyll, MissingH, pandoc, stdenv, text, time
}:
mkDerivation {
  pname = "robertwpearce-com";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base hakyll MissingH pandoc text time
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
