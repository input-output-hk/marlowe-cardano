{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
    flags = { developer = false; };
    package = {
      specVersion = "1.8";
      identifier = { name = "resource-pool"; version = "0.2.3.2"; };
      license = "BSD-3-Clause";
      copyright = "Copyright 2011 MailRank, Inc.";
      maintainer = "Bryan O'Sullivan <bos@serpentine.com>,\nBas van Dijk <v.dijk.bas@gmail.com>";
      author = "Bryan O'Sullivan <bos@serpentine.com>";
      homepage = "http://github.com/bos/pool";
      url = "";
      synopsis = "A high-performance striped resource pooling implementation";
      description = "A high-performance striped pooling abstraction for managing\nflexibly-sized collections of resources such as database\nconnections.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/resource-pool-0.2.3.2.tar.gz";
      sha256 = "8627eea2bea8824af2723646e74e2af0c73f583dd0c496c9fd242cd9d242bc12";
      });
    }) // {
    package-description-override = "name:                resource-pool\nversion:             0.2.3.2\nsynopsis:            A high-performance striped resource pooling implementation\ndescription:\n  A high-performance striped pooling abstraction for managing\n  flexibly-sized collections of resources such as database\n  connections.\n\nhomepage:            http://github.com/bos/pool\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Bryan O'Sullivan <bos@serpentine.com>\nmaintainer:          Bryan O'Sullivan <bos@serpentine.com>,\n                     Bas van Dijk <v.dijk.bas@gmail.com>\ncopyright:           Copyright 2011 MailRank, Inc.\ncategory:            Data, Database, Network\nbuild-type:          Simple\nextra-source-files:\n  README.markdown\n\ncabal-version:       >=1.8\n\nflag developer\n  description: operate in developer mode\n  default: False\n  manual: True\n\nlibrary\n  exposed-modules:\n    Data.Pool\n\n  build-depends:\n    base >= 4.4 && < 5,\n    hashable,\n    monad-control >= 0.2.0.1,\n    transformers,\n    transformers-base >= 0.4,\n    stm >= 2.3,\n    time,\n    vector >= 0.7\n\n  if flag(developer)\n    ghc-options: -Werror\n    ghc-prof-options: -auto-all\n    cpp-options: -DASSERTS -DDEBUG\n\n  ghc-options: -Wall\n\nsource-repository head\n  type:     git\n  location: http://github.com/bos/pool\n\nsource-repository head\n  type:     mercurial\n  location: http://bitbucket.org/bos/pool\n";
    }