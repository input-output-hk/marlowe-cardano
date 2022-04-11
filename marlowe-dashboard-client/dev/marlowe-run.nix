{ chain-index-port, wallet-port }:
{
  wbeConfig = { host = "wallet"; port = wallet-port; };
  chainIndexConfig = { host = "chain-index"; port = chain-index-port; };
  staticPath = "/var/empty";
  verbosity = 3;
}
