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
      identifier = { name = "eventuo11y-json"; version = "0.2.0.0"; };
      license = "Apache-2.0";
      copyright = "Copyright 2022 Shea Levy.";
      maintainer = "shea@shealevy.com";
      author = "Shea Levy";
      homepage = "";
      url = "";
      synopsis = "aeson-based rendering for eventuo11y";
      description = "Render [eventuo11y](https://hackage.haskell.org/package/eventuo11y) 'Observe.Event.Event's\nto JSON.\n\nSee \"Observe.Event.Dynamic\" for 'Observe.Event.Event' selectors that don't require\ngenerating domain-specific types and renderers.\n\nSee \"Observe.Event.Render.JSON\" for renderer types.\n\nSee \"Observe.Event.Render.JSON.DSL.Compile\" to compile the \"Observe.Event.DSL\" DSL\nin a way that generates \"Observe.Event.Render.JSON\" renderers.\n\nSee \"Observe.Event.Render.JSON.Handle\" for rendering 'Observe.Event.Event's as\nJSON to a 'System.IO.Handle', and in particular to 'System.IO.stderr'.";
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
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."eventuo11y" or (errorHandler.buildDepError "eventuo11y"))
          (hsPkgs."eventuo11y-dsl" or (errorHandler.buildDepError "eventuo11y-dsl"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."uuid" or (errorHandler.buildDepError "uuid"))
          ];
        buildable = true;
        modules = [
          "Observe/Event/Dynamic"
          "Observe/Event/Render/JSON"
          "Observe/Event/Render/JSON/DSL/Compile"
          "Observe/Event/Render/JSON/Handle"
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
    postUnpack = "sourceRoot+=/eventuo11y-json; echo source root reset to $sourceRoot";
    }