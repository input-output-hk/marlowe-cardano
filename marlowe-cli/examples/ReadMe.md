# Examples for Marlowe CLI Tool


Also see the [Marlowe Contract Cookbook](../cookbook/ReadMe.md) for further examples of running contracts using `marlowe-cli`.


## Examples

*   [simple contract](simple/ReadMe.md)
*   [escrow](escrow/ReadMe.md)
*   [swap](swap/ReadMe.md)
*   [zero-coupon bond](zcb/ReadMe.md)
*   [contract for differences](cfd/ReadMe.md)
*   [covered call](coveredCall/ReadMe.md)


## Testing

*   [simple contract](simple/run-test.sh)
*   escrow
    *   ["everything is alright"](escrow/run-everything-is-alright.sh)
    *   ["confirm problem"](escrow/run-confirm-problem.sh)
    *   ["dismiss claim"](escrow/run-dimiss-claim.sh)
    *   ["confirm claim"](escrow/run-confirm-claim.sh)
*   [swap](swap/run-swap.sh)
*   [zero-coupon bond](zcb/run-zcb.sh)
*   [contract for differences](cfd/run-cfd.sh)
*   [covered call](coveredCall/run-coveredCall.sh)


## Generating docs

Doc generation runs the test scripts to produce logs and then processes the
logs. Make sure these variables have been set

    $ export FAUCET_ADDRESS=...
    $ export FAUCET_SKEY_FILE=...

To generate docs for `examples/simple`

    $ cd examples/simple
    $ ./Makefile docs

Repeat this for each test dir in `examples` as needed.
