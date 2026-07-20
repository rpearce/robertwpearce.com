{ mkDerivation
, base
, containers
, filepath
, hakyll
, lib
, pandoc
, pandoc-types
, slugger
, tagsoup
, text
, time
, time-locale-compat
}:
mkDerivation {
  pname = "ssg";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base
    containers
    filepath
    hakyll
    pandoc
    pandoc-types
    slugger
    tagsoup
    text
    time
    time-locale-compat
  ];
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
  mainProgram = "hakyll-site";
}
