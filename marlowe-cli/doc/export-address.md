# Marlowe CLI: Address Information

The `marlowe-cli contract address` command computes the address of the Marlowe validator.


## Options

    $ marlowe-cli contract address --help
    
    Usage: marlowe-cli contract address [--testnet-magic INTEGER]
                                        [--slot-length INTEGER]
                                        [--slot-offset INTEGER]
                                        [--stake-address ADDRESS]
                                        [--roles-currency CURRENCY_SYMBOL]
      Print a contract validator address.
    
    Available options:
      --testnet-magic INTEGER           Network magic, or omit for mainnet.
      --stake-address ADDRESS           Stake address, if any.
      --roles-currency CURRENCY_SYMBOL  The currency symbol for roles, if any.
      -h,--help                         Show this help text

The stake address can be omitted if no staking will be done at the script address. If the currency symbol is omitted, then ADA is used as the currency for the Marlowe roles.


# Example

The following commands compute the address of the default Marlowe validator for the public testnet.

    $ marlowe-cli contract address --testnet-magic 1097911063                                                  \
                                   --stake-address stake1uyx07tvec6fuff78s7v456fx94ukmpvh4x6tynjhmqwta8c09uy75 \
                                   --roles-currency 8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d
    
    addr_test1zrmpz9dkw49gqm0ax2mgy4svp7u3qtmqhgrzzg5hxntdjecvluken35ncjnu0puetf5jvttedkze02d5kf890kquh60syyc4uy
    
    
    $ marlowe-cli contract address --testnet-magic 1097911063
    
    addr_test1wqqwzc3j8jrqfn0scn8ydm5mesla5xrg3mqpcmp8hjplt6q2ng95l
