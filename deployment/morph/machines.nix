{ pkgs, mkMachine, tfinfo }:
{
  # The network attribute allows to supply
  # some settings to all deployments
  network = {
    description = "plutus network";
    inherit pkgs;
  };

  "${tfinfo.marloweDashA.dns}" = mkMachine {
    name = "marloweDashA";
    config.networking = {
      firewall.enable = true;
      firewall.allowedTCPPorts = [ 22 80 9080 ];
      timeServers = [ "1.amazon.pool.ntp.org" "2.amazon.pool.ntp.org" "3.amazon.pool.ntp.org" ];
    };
    config.services.nginx =
      {
        enable = true;
        recommendedGzipSettings = true;
        recommendedProxySettings = true;
        recommendedOptimisation = true;

        virtualHosts = {
          "marlowe-dash" = {
            listen = [{ addr = "0.0.0.0"; port = 9080; }];
            globalRedirect = "marlowe-run-staging.plutus.aws.iohkdev.io";
          };
        };
      };
  };

  "${tfinfo.playgroundsA.dns}" = mkMachine {
    name = "playgroundsB";
    config.networking = {
      firewall.enable = true;
      firewall.allowedTCPPorts = [ 22 80 8181 9080 ];
      timeServers = [ "1.amazon.pool.ntp.org" "2.amazon.pool.ntp.org" "3.amazon.pool.ntp.org" ];
    };
    config.services.nginx =
      {
        enable = true;
        recommendedGzipSettings = true;
        recommendedProxySettings = true;
        recommendedOptimisation = true;

        virtualHosts = {
          "marlowe-web" = {
            listen = [{ addr = "0.0.0.0"; port = 8181; }];
            globalRedirect = "marlowe-website-staging.plutus.aws.iohkdev.io";
          };
          "marlowe-playground" = {
            listen = [{ addr = "0.0.0.0"; port = 9080; }];
            globalRedirect = "marlowe-playground-staging.plutus.aws.iohkdev.io";
          };
        };
      };
  };

  "${tfinfo.webghcA.dns}" = mkMachine {
    name = "webghcA";
    config = { };
  };
}
