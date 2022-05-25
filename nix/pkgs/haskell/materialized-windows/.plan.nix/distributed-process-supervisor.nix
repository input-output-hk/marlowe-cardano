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
      identifier = {
        name = "distributed-process-supervisor";
        version = "0.2.1";
        };
      license = "BSD-3-Clause";
      copyright = "Tim Watson 2012 - 2013";
      maintainer = "Tim Watson <watson.timothy@gmail.com>";
      author = "Tim Watson";
      homepage = "http://github.com/haskell-distributed/distributed-process-supervisor";
      url = "";
      synopsis = "Supervisors for The Cloud Haskell Application Platform";
      description = "A part of the Cloud Haskell framework\nThis package implements a process which supervises a set of other processes, referred to as its children.\nThese child processes can be either workers (i.e., processes that do something useful in your application)\nor other supervisors. In this way, supervisors may be used to build a hierarchical process structure\ncalled a supervision tree, which provides a convenient structure for building fault tolerant software.\nFor detailed information see \"Control.Distributed.Process.Supervisor\"";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "ChangeLog" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."data-accessor" or (errorHandler.buildDepError "data-accessor"))
          (hsPkgs."distributed-static" or (errorHandler.buildDepError "distributed-static"))
          (hsPkgs."distributed-process" or (errorHandler.buildDepError "distributed-process"))
          (hsPkgs."distributed-process-extras" or (errorHandler.buildDepError "distributed-process-extras"))
          (hsPkgs."distributed-process-client-server" or (errorHandler.buildDepError "distributed-process-client-server"))
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
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          ];
        buildable = true;
        modules = [
          "Control/Distributed/Process/Supervisor/Types"
          "Control/Distributed/Process/Supervisor"
          "Control/Distributed/Process/Supervisor/Management"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "SupervisorTests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."distributed-static" or (errorHandler.buildDepError "distributed-static"))
            (hsPkgs."distributed-process" or (errorHandler.buildDepError "distributed-process"))
            (hsPkgs."distributed-process-supervisor" or (errorHandler.buildDepError "distributed-process-supervisor"))
            (hsPkgs."distributed-process-extras" or (errorHandler.buildDepError "distributed-process-extras"))
            (hsPkgs."distributed-process-client-server" or (errorHandler.buildDepError "distributed-process-client-server"))
            (hsPkgs."distributed-static" or (errorHandler.buildDepError "distributed-static"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."data-accessor" or (errorHandler.buildDepError "data-accessor"))
            (hsPkgs."fingertree" or (errorHandler.buildDepError "fingertree"))
            (hsPkgs."network-transport" or (errorHandler.buildDepError "network-transport"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."network-transport-tcp" or (errorHandler.buildDepError "network-transport-tcp"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."rematch" or (errorHandler.buildDepError "rematch"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            ];
          buildable = true;
          modules = [ "TestUtils" ];
          hsSourceDirs = [ "tests" ];
          mainPath = [ "TestSupervisor.hs" ];
          };
        "NonThreadedSupervisorTests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."distributed-static" or (errorHandler.buildDepError "distributed-static"))
            (hsPkgs."distributed-process" or (errorHandler.buildDepError "distributed-process"))
            (hsPkgs."distributed-process-supervisor" or (errorHandler.buildDepError "distributed-process-supervisor"))
            (hsPkgs."distributed-process-extras" or (errorHandler.buildDepError "distributed-process-extras"))
            (hsPkgs."distributed-process-client-server" or (errorHandler.buildDepError "distributed-process-client-server"))
            (hsPkgs."distributed-static" or (errorHandler.buildDepError "distributed-static"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."data-accessor" or (errorHandler.buildDepError "data-accessor"))
            (hsPkgs."fingertree" or (errorHandler.buildDepError "fingertree"))
            (hsPkgs."network-transport" or (errorHandler.buildDepError "network-transport"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."network-transport-tcp" or (errorHandler.buildDepError "network-transport-tcp"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."rematch" or (errorHandler.buildDepError "rematch"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            ];
          buildable = true;
          modules = [ "TestUtils" ];
          hsSourceDirs = [ "tests" ];
          mainPath = [ "TestSupervisor.hs" ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "4";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "4";
      rev = "minimal";
      sha256 = "";
      };
    }