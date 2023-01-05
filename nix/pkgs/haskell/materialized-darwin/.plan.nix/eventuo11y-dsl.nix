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
      identifier = { name = "eventuo11y-dsl"; version = "0.2.0.0"; };
      license = "Apache-2.0";
      copyright = "Copyright 2022 Shea Levy.";
      maintainer = "shea@shealevy.com";
      author = "Shea Levy";
      homepage = "";
      url = "";
      synopsis = "DSL for defining eventuo11y fields and selectors";
      description = "Exposes a DSL for low-boilerplate definition of [eventuo11y](https://hackage.haskell.org/package/eventuo11y) fields and selectors.\n\nSee \"Observe.Event.DSL\" for the core DSL.\n\nSee \"Observe.Event.DSL.Compile\" for the TemplateHaskell code to generate the field and selector types.\n\nSee [Example.hs](https://github.com/shlevy/eventuo11y/tree/v0.5.0.0/Example.hs) for an example.\n\nPackages providing @EventBackend@s should likely also provide extensions to the DSL and generate default renderers.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "CHANGELOG.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          ];
        buildable = true;
        modules = [
          "Observe/Event/DSL"
          "Observe/Event/DSL/Compile"
          "Observe/Event/Syntax"
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
    postUnpack = "sourceRoot+=/eventuo11y-dsl; echo source root reset to $sourceRoot";
    }