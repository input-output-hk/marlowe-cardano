{ supportedSystems
, rootsOnly
, checkMaterialization
, inputs
, source-repo-override
, pkgs
, internal
, evalSystem
, ci-lib
}:
let
  inherit (ci-lib) dimension platformFilterGeneric filterAttrsOnlyRecursive filterSystems;
  # limit supportedSystems to what the CI can actually build
  # currently that is linux and darwin.
  systems = filterSystems supportedSystems;
  crossSystems = { };

  # Collects haskell derivations and builds an attrset:
  #
  # { library = { ... }
  # , tests = { ... }
  # , benchmarks = { ... }
  # , exes = { ... }
  # , checks = { ... }
  # }
  #  Where each attribute contains an attribute set
  #  with all haskell components of that type
  mkHaskellDimension = pkgs: haskellProjects:
    let
      # retrieve all checks from a Haskell package
      collectChecks = _: ps: pkgs.haskell-nix.haskellLib.collectChecks' ps;
      # retrieve all components of a Haskell package
      collectComponents = type: ps: pkgs.haskell-nix.haskellLib.collectComponents' type ps;
      # Given a component type and the retrieve function, retrieve components from haskell packages
      select = type: selector: (selector type) haskellProjects;
      # { component-type : retriever-fn }
      attrs = {
        "library" = collectComponents;
        "tests" = collectComponents;
        "benchmarks" = collectComponents;
        "exes" = collectComponents;
        "checks" = collectChecks;
      };
    in
    dimension "Haskell component" attrs select;

  # Collects all project derivations to build grouped by system:
  #
  # { linux = { ... }
  # , darwin = { ... }
  # }
  mkSystemDimension = systems:
    let
      # given a system ("x86_64-linux") return an attrset of derivations to build
      _select = _: system: crossSystem:
        let
          packages = internal.packagesFun { inherit system crossSystem checkMaterialization source-repo-override evalSystem; };
          pkgs = packages.pkgs;
          marlowe = packages.marlowe;
          # Map `crossSystem.config` to a name used in `lib.platforms`
          platformString =
            if crossSystem == null then system
            else if crossSystem.config == "x86_64-w64-mingw32" then "x86_64-windows"
            else crossSystem.config;
          isBuildable = platformFilterGeneric pkgs platformString;
          filterCross = x:
            if crossSystem == null
            then x
            else {
              # When cross compiling only include haskell for now
              inherit (x) haskell;
            };
        in
        filterAttrsOnlyRecursive (_: drv: isBuildable drv) ({
          # The haskell.nix IFD roots for the Haskell project. We include these so they won't be GCd and will be in the
          # cache for users
          inherit (marlowe.haskell.project) roots;
        } // pkgs.lib.optionalAttrs (system == "x86_64-linux") {
          inherit (packages) compose-spec;
        } // pkgs.lib.optionalAttrs (!rootsOnly) (filterCross {
          # build relevant top level attributes from flake.nix
          inherit (packages) tests;

          # Build the shell expression to be sure it works on all platforms
          #
          # The shell should never depend on any of our Haskell packages, which can
          # sometimes happen by accident. In practice, everything depends transitively
          # on 'plutus-core', so this does the job.
          # FIXME: this should simply be set on the main shell derivation, but this breaks
          # lorri: https://github.com/target/lorri/issues/489. In the mean time, we set it
          # only on the CI version, so that we still catch it, but lorri doesn't see it.
          shell = (import ./dev-shell.nix { inherit packages system; }).overrideAttrs (attrs: attrs // {
            disallowedRequisites = [ marlowe.haskell.packages.plutus-core.components.library ];
          });

          # build all haskell packages and tests
          haskell = pkgs.recurseIntoAttrs (mkHaskellDimension pkgs marlowe.haskell.projectPackages);
        }));
    in
    dimension "System" systems (name: sys: _select name sys null)
    // dimension "Cross System" crossSystems (name: crossSys: _select name "x86_64-linux" crossSys);
in
mkSystemDimension systems
