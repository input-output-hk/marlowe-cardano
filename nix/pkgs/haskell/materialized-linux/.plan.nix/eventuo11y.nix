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
      specVersion = "3.0";
      identifier = { name = "eventuo11y"; version = "0.6.0.0"; };
      license = "Apache-2.0";
      copyright = "Copyright 2022 Shea Levy.";
      maintainer = "shea@shealevy.com";
      author = "Shea Levy";
      homepage = "";
      url = "";
      synopsis = "An event-oriented observability library";
      description = "Instrument your Haskell codebase with wide, semantically meaningful events.\nThis library is designed with separating the following concerns in mind:\n\n[@Writing instrumentation@] When instrumenting code, I want to think in terms of my\napplication domain and report any information I might need to infer internal\napplication-level state and understand the behavior of my program/library.\n\n[@Consuming instrumentation@] When consuming instrumentation, I want to think in\nterms of the API fo the specific backend I'm supporting (writing to @stderr@,\nserving a @Prometheus@ page, posting to @OpenTelemetry@) and what is needed to\nrender to that API.\n\n[@Initializing instrumentation in an application@] When I'm ready to tie it all\ntogether, I want to identify the specific backends I want to post to and provide\nthe bridge code to render the domain-specific instrumentation as needed for those\nbackends. I also want to handle concerns like sampling or client-side aggregation\nof domain-specific instrumentation to keep usage manageable.\n\nSee \"Observe.Event\" for detailed documentation on instrumenting your code.\n\nSee \"Observe.Event.Backend\" for documentation on writing an\n@EventBackend@.\n\nSee [eventuo11y-dsl](https://hackage.haskell.org/package/eventuo11y-dsl) for simpler syntax for\ncreating application-level instrumentation types.\n\nSee [eventuo11y-json](https://hackage.haskell.org/package/eventuo11y-json) for JSON-based rendering\nand backends.\n\nSee [Example.hs](https://github.com/shlevy/eventuo11y/tree/v0.5.0.0/Example.hs) for an example.\n\nSee [eventuo11y-batteries](https://hackage.haskell.org/package/eventuo11y-batteries) for miscellaneous\nframework-specific helpers.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "CHANGELOG.md" "Example.hs" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
          (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
          ];
        buildable = true;
        modules = [
          "Control/Monad/Cleanup"
          "Observe/Event"
          "Observe/Event/Backend"
          "Observe/Event/BackendModification"
          ];
        hsSourceDirs = [ "src" ];
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