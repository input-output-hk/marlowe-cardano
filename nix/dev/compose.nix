{ sqitchPg, runCommand, writeShellScriptBin, writeText, lib, glibcLocales, postgresql }:
let
  run-sqitch = writeShellScriptBin "run-sqitch" ''
    export PATH="PATH:${lib.makeBinPath [ sqitchPg postgresql ]}"
    export LOCALE_ARCHIVE="${glibcLocales}/lib/locale/locale-archive"
    cd /src/marlowe-chain-sync
    exec sqitch deploy
  '';

  symlinks = runCommand "symlinks" { } ''
    mkdir -p $out
    ln -sv ${run-sqitch}/bin/run-sqitch $out
  '';

  spec = {
    services.postgres = {
      image = "postgres:11.5-alpine";

      # TODO translate from attrset
      environment = [
        "POSTGRES_LOGGING=true"
        "POSTGRES_USER=postgres"
        "POSTGRES_HOST_AUTH_METHOD=trust"
        "TZ=UTC"
      ];

      # TODO dynamic port alloc
      # TODO strucutred port definition
      ports = [
        "\${POSTGRES_PORT:-5432}:5432"
      ];

      # TODO strucutred volume definition
      # TODO Connect volumes here to top-level volumes
      volumes = [
        "postgres:/var/lib/postgresql/data"
        "./postgres/init.sql:/docker-entrypoint-initdb.d/init.sql"
        "./:/src"
        "/nix:/nix"
        "${symlinks}:/exec"
      ];

      # TODO enum
      restart = "unless-stopped";

      # this should be in the dockerfile...
      healthcheck = {
        test = [ "CMD" "pg_isready" "-U" "postgres" ];
        # TODO structured
        interval = "10s";
        timeout = "5s";
        retries = 5;
      };

      logging = {
        # enum
        driver = "json-file";
        options = {
          max-size = "200k";
          max-file = "10";
        };
      };
    };

    volumes.postgres = null;
  };
in
writeText "compose.yaml" (builtins.toJSON spec)
