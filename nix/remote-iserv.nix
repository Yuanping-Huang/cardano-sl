{ system
, compiler
, flags
, pkgs
, hsPkgs
, pkgconfPkgs
, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = {
        name = "remote-iserv";
        version = "8.5";
      };
      license = "BSD-3-Clause";
      copyright = "XXX";
      maintainer = "XXX";
      author = "XXX";
      homepage = "";
      url = "";
      synopsis = "iserv allows GHC to delegate Tempalte Haskell computations";
      description = "";
      buildType = "Simple";
    };
    components = {
      exes = {
        "remote-iserv" = {
          depends  = [
            (hsPkgs.base)
            (hsPkgs.libiserv)
          ];
        };
      };
    };
  } // rec { src = ./remote-iserv-8.4.4.tar.gz; }
