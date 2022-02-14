{ wallet-port }:
{
  wbeConfig = { host = "wallet"; port = wallet-port; };
  staticPath = "/var/empty";
  verbosity = 3;
}
