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
      identifier = { name = "moo"; version = "1.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Sergey Astanin <s.astanin@gmail.com>";
      author = "Sergey Astanin <s.astanin@gmail.com>";
      homepage = "http://www.github.com/astanin/moo/";
      url = "";
      synopsis = "Genetic algorithm library";
      description = "Moo library provides building blocks to build custom\ngenetic algorithms in Haskell. They can be used to\nfind solutions to optimization and search problems.\n\nVariants supported out of the box: binary (using\nbit-strings) and continuous (real-coded).\nPotentially supported variants: permutation,\ntree, hybrid encodings (require customizations).\n\nBinary GAs: binary and Gray encoding; point mutation;\none-point, two-point, and uniform crossover.\nContinuous GAs: Gaussian mutation; BLX-α, UNDX, and\nSBX crossover.\nSelection operators: roulette, tournament, and\nstochastic universal sampling (SUS);\nwith optional niching, ranking, and scaling.\nReplacement strategies: generational with elitism\nand steady state.\nConstrained optimization: random constrained\ninitialization, death penalty, constrained\nselection without a penalty function.\nMulti-objective optimization: NSGA-II\nand constrained NSGA-II.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [
        "README.md"
        "examples/README.md"
        "examples/ExampleMain.hs"
        "examples/beale.hs"
        "examples/cp_himmelblau.hs"
        "examples/cp_sphere2.hs"
        "examples/knapsack.hs"
        "examples/mop_constr2.hs"
        "examples/mop_kursawe.hs"
        "examples/mop_minsum_maxprod.hs"
        "examples/rosenbrock.hs"
        "examples/schaffer2.hs"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."MonadRandom" or (errorHandler.buildDepError "MonadRandom"))
          (hsPkgs."mersenne-random-pure64" or (errorHandler.buildDepError "mersenne-random-pure64"))
          (hsPkgs."gray-code" or (errorHandler.buildDepError "gray-code"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."random-shuffle" or (errorHandler.buildDepError "random-shuffle"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."parallel" or (errorHandler.buildDepError "parallel"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          ];
        buildable = true;
        modules = [
          "Moo/GeneticAlgorithm/Crossover"
          "Moo/GeneticAlgorithm/LinAlg"
          "Moo/GeneticAlgorithm/Multiobjective/NSGA2"
          "Moo/GeneticAlgorithm/Multiobjective/Types"
          "Moo/GeneticAlgorithm/Multiobjective/Metrics"
          "Moo/GeneticAlgorithm/Selection"
          "Moo/GeneticAlgorithm/StopCondition"
          "Moo/GeneticAlgorithm/Utilities"
          "Moo/GeneticAlgorithm/Niching"
          "Moo/GeneticAlgorithm"
          "Moo/GeneticAlgorithm/Binary"
          "Moo/GeneticAlgorithm/Constraints"
          "Moo/GeneticAlgorithm/Continuous"
          "Moo/GeneticAlgorithm/Multiobjective"
          "Moo/GeneticAlgorithm/Random"
          "Moo/GeneticAlgorithm/Run"
          "Moo/GeneticAlgorithm/Statistics"
          "Moo/GeneticAlgorithm/Types"
          ];
        };
      tests = {
        "moo-tests" = {
          depends = [
            (hsPkgs."moo" or (errorHandler.buildDepError "moo"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."random-shuffle" or (errorHandler.buildDepError "random-shuffle"))
            (hsPkgs."MonadRandom" or (errorHandler.buildDepError "MonadRandom"))
            (hsPkgs."mersenne-random-pure64" or (errorHandler.buildDepError "mersenne-random-pure64"))
            (hsPkgs."gray-code" or (errorHandler.buildDepError "gray-code"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."parallel" or (errorHandler.buildDepError "parallel"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            ];
          buildable = true;
          modules = [
            "Tests/Common"
            "Tests/Internals/TestControl"
            "Tests/Internals/TestCrossover"
            "Tests/Internals/TestFundamentals"
            "Tests/Internals/TestMultiobjective"
            "Tests/Internals/TestSelection"
            "Tests/Internals/TestConstraints"
            "Tests/Problems/Rosenbrock"
            "Moo/GeneticAlgorithm"
            "Moo/GeneticAlgorithm/Binary"
            "Moo/GeneticAlgorithm/Constraints"
            "Moo/GeneticAlgorithm/Continuous"
            "Moo/GeneticAlgorithm/Crossover"
            "Moo/GeneticAlgorithm/Niching"
            "Moo/GeneticAlgorithm/Run"
            "Moo/GeneticAlgorithm/Random"
            "Moo/GeneticAlgorithm/Utilities"
            "Moo/GeneticAlgorithm/LinAlg"
            "Moo/GeneticAlgorithm/Multiobjective"
            "Moo/GeneticAlgorithm/Multiobjective/NSGA2"
            "Moo/GeneticAlgorithm/Multiobjective/Types"
            "Moo/GeneticAlgorithm/Multiobjective/Metrics"
            "Moo/GeneticAlgorithm/Selection"
            "Moo/GeneticAlgorithm/Statistics"
            "Moo/GeneticAlgorithm/StopCondition"
            "Moo/GeneticAlgorithm/Types"
            ];
          mainPath = [ "moo-tests.hs" ];
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