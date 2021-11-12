# Marlowe CLI: Address Information

The `address` command computes the address of the Marlowe validator.


## Options

    $ marlowe-cli address --help
    
    Usage: marlowe-cli address [--testnet-magic INTEGER] [--stake-address ADDRESS]
      Print a validator address.
    
    Available options:
      --testnet-magic INTEGER  Network magic, or omit for mainnet.
      --stake-address ADDRESS  Stake address, if any.
      -h,--help                Show this help text

The stake address can be omitted if no staking will be done at the script address.


# Example

The following commands compute the address of the default Marlowe validator for the public testnet.

    $ marlowe-cli address --testnet-magic 1097911063
    
    addr_test1wqqwzc3j8jrqfn0scn8ydm5mesla5xrg3mqpcmp8hjplt6q2ng95l
    
    
    $ marlowe-cli address --testnet-magic 1097911063 \
                          --stake-address stake1uyx07tvec6fuff78s7v456fx94ukmpvh4x6tynjhmqwta8c09uy75
    
    addr_test1zqqwzc3j8jrqfn0scn8ydm5mesla5xrg3mqpcmp8hjplt6qvluken35ncjnu0puetf5jvttedkze02d5kf890kquh60sa5y78q
