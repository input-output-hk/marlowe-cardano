{ chain-index-port, wallet-port, wallet-host, chain-index-host }:
{
  wbeConfig = { host = wallet-host; port = wallet-port; };
  chainIndexConfig = { host = chain-index-host; port = chain-index-port; };
  staticPath = "/var/empty";
  verbosity = 3;
}
