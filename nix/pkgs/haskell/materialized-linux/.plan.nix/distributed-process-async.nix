{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = {};
    package = {
      specVersion = "1.8";
      identifier = { name = "distributed-process-async"; version = "0.2.6"; };
      license = "BSD-3-Clause";
      copyright = "Tim Watson 2012 - 2016";
      maintainer = "Tim Watson <watson.timothy@gmail.com>";
      author = "Tim Watson";
      homepage = "http://github.com/haskell-distributed/distributed-process-async";
      url = "";
      synopsis = "Cloud Haskell Async API";
      description = "This package provides a higher-level interface over Processes, in which an Async a is a\nconcurrent, possibly distributed Process that will eventually deliver a value of type a.\nThe package provides ways to create Async computations, wait for their results, and cancel them.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENCE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."data-accessor" or (errorHandler.buildDepError "data-accessor"))
          (hsPkgs."distributed-process" or (errorHandler.buildDepError "distributed-process"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."fingertree" or (errorHandler.buildDepError "fingertree"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        modules = [
          "Control/Distributed/Process/Async/Internal/Types"
          "Control/Distributed/Process/Async"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "AsyncTests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
            (hsPkgs."distributed-process" or (errorHandler.buildDepError "distributed-process"))
            (hsPkgs."distributed-process-async" or (errorHandler.buildDepError "distributed-process-async"))
            (hsPkgs."distributed-process-systest" or (errorHandler.buildDepError "distributed-process-systest"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."network-transport" or (errorHandler.buildDepError "network-transport"))
            (hsPkgs."network-transport-tcp" or (errorHandler.buildDepError "network-transport-tcp"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."rematch" or (errorHandler.buildDepError "rematch"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          hsSourceDirs = [ "tests" ];
          mainPath = [ "TestAsync.hs" ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "2";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "2";
      rev = "minimal";
      sha256 = "";
      };
    }