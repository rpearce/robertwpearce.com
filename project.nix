{ mkDerivation, base, hakyll, pandoc, stdenv, text, time }:
mkDerivation {
  pname = "robertwpearce-com";
  version = "0.1.0.2";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base hakyll pandoc text time ];
  homepage = "https://github.com/rpearce/robertwpearce.com";
  description = "💾 My personal site; built with hakyll";
  license = stdenv.lib.licenses.bsd3;
}
