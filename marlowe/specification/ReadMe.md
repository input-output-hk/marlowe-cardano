# On-Chain Transaction Specification for Marlowe


## Scope

This document defines the specification for Marlowe semantics's interface with the Cardano blockchain. Marlowe utilizes three Plutus scripts: (i) a monetary policy for the roles currency used in some Marlowe contracts; (ii) the Marlowe application validator script that enforces Marlowe semantics; and (iii) the Marlowe payout-validator script that allows the holder of a role token to withdraw funds paid by the Marlowe application.


## Participants in Marlowe Contracts

Marlowe contracts identify each participant by either a *public-key hash (PKH)* or a *role*. Marlowe semantics allow PKHs and roles to be freely mixed in contract logic.
- A Marlowe PKH corresponds to the hash of a Cardano payment verification key, `Hash PaymentKey`.
- A Marlowe role corresponds to a Cardano native token with the `TokenName`  for the role. (In Cardano, role names may be zero to thirty-two bytes long.) All of the role tokens in a given Marlowe contract must use the same  `CurrencySymbol` .


## Life Cycle of a Marlowe Contract

The execution of a Marlowe contract proceeds as a linear sequence of applications of inputs at the contract's script address.
1. Role tokens are typically minted prior to or within the creation transaction of the contract, though this is not enforced on-chain.
2. The creation transaction for the contract stores the state of the contract in the datum at the contract's script address.
3. Each transaction that interacts with the contract updates the state/datum at that same address.
4. When a contract closes, there is no output to the contract's script address.
5. If the contract pays funds to a role during application of inputs, those funds are sent to the address of the Marlowe payout-validator script.

Thus each Marlowe contract is a finite linear sequence of continuations at the script address, from creation to closure.


## Monetary Policy for Role Tokens

Any Cardano monetary policy may be used to mint the role tokens used in a Marlowe contract. For security in standard Marlowe use cases, a one-time or locked minting policy such as `Plutus.Contracts.Currency.OneShotCurrency` is recommended. Exotic use cases might employ other monetary policies. It is the responsibility both of the developer of the off-chain code managing a contract and also of the user of the contract to verify that the monetary policy of the role tokens meets their security requirements.


## Representation of Marlowe Semantics in Plutus

Marlowe semantics are embodied in the Haskell function `computeTransaction` that follows [the Isabelle implementation of the Marlowe DSL](https://github.com/input-output-hk/marlowe/tree/master/isabelle/Core). Note that `computeTransaction` uses the `PlutusTx` types and functions instead of the standard-prelude or GHC-base Haskell ones.
```haskell
computeTransaction :: TransactionInput -> State -> Contract -> TransactionOutput
```

With the exception of `Input`, [the Isabelle types for Marlowe](https://github.com/input-output-hk/marlowe/blob/master/isabelle/Core/SemanticsTypes.thy) directly correspond to their Haskell representations, so below we only provide the definitions that will be later referenced in this document.
```haskell
data TransactionInput =
    TransactionInput
    {
      txInterval :: TimeInterval
    , txInputs   :: [Input]
    }

type TimeInterval = (POSIXTime, POSIXTime)

data State =
    State
    {
      accounts    :: Accounts
    , choices     :: Map ChoiceId ChosenNum
    , boundValues :: Map ValueId Integer
    , minTime     :: POSIXTime
    }

type Accounts = Map (AccountId, Token) Integer

data TransactionOutput =
    TransactionOutput
    {
      txOutWarnings :: [TransactionWarning]
    , txOutPayments :: [Payment]
    , txOutState    :: State
    , txOutContract :: Contract
    }
  | Error TransactionError
```

The `Input` of the Isabelle definition corresponds to the `InputContent` of the Haskell representation because the latter includes support for merkleization of contract continuations in `Case` statements. Note that Merkleization is not currently treated by the Isabelle specification of Marlowe.
```haskell
data Input =
    NormalInput InputContent
  | MerkleizedInput InputContent BuiltinByteString Contract
```
In the above, the `BuiltinByteString` is the hash of the serialized continuation `Contract`.


## Plutus Validator for Marlowe Semantics

The Marlowe validator is an unparameterized interpreter of Marlowe semantics. Thus, the script address of the Marlowe validator is independent of the particular contract and roles currency.
```haskell
smallMarloweValidator :: MarloweData -> MarloweInput -> ScriptContext -> Bool
```


### Types

The Plutus `Datum` for Marlowe simply bundles the role-currency and payout information with the `State` and the `Contract`:
```haskell
data MarloweData =
    MarloweData
    {
      marloweParams   :: MarloweParams
    , marloweState    :: State
    , marloweContract :: Contract
    }

data MarloweParams =
    MarloweParams
    {
      rolePayoutValidatorHash :: ValidatorHash
    , rolesCurrency           :: CurrencySymbol
    }
```

The Plutus `Redeemer` provides the inputs for the semantics:
```haskell
type MarloweInput = [MarloweTxInput]

data MarloweTxInput =
    Input InputContent
  | MerkleizedTxInput InputContent BuiltinByteString
```
In the above, the `BuiltinByteString` is the hash of the serialized continuation of the contract. If `MerkleizedTxInput` is supplied in a redeemer, then the `ScriptContext` for the transaction must also contain an extra entry in its `txInfoData . scriptContextTxInfo` map from `DatumHash` to `Datum` for the serialized continuation of the contract.


### Relationship between Marlowe Validator and Semantics

The arguments of `computeTransaction` must be constructed as follows:
1. The `txInterval` of `TransactionInput` is derived from the `txInfoValidTimeRange . scriptContextInfo` of the `ScriptContext`, as detailed below.
2. The `txInputs` of `TransactionInput` is derived from the `MarloweInput` provided as the `Redeemer` and the `txInfoData . scriptContextTxInfo` of the `ScriptContext`, as detailed below.
3. The `State` is the `marloweState` of the `MarloweData` provided as the `Datum`.
4. The `Contract` is the `marloweContract` of the `MarloweData` provided as the `Datum`.
5. The new `Datum` at the script address is the `MarloweData` with the same `marloweParams` as originally, but with the new `txOutState` and `txOutContract` of the `TransactionOutput`.

![Relationship between Marlowe validator and semantics.](semantics2plutus.svg)


### Specification

Consider the application of the Marlowe validator and Marlowe semantics:
```haskell
validationResult = smallMarloweValidator marloweData marloweInput scriptContext

transactionOutput = computeTransaction transactionInput inState inContract
```

Furthermore, let `marloweValidatorHash :: ValidatorHash` be the hash of the Marlowe semantics script.

The validation fails (via returning `False` for `validationResult` or via the throwing of an error) if any of the following constraints does not hold.


#### *Constraint 1.* Typed validation

The datum, redeemer, and script context deserialize to the correct types.

Let `datum :: BuiltinData` and `redeemer :: BuiltinData` be the datum and redeemer in the script witness for spending the Marlowe UTxO, and let `scriptContext' :: BuiltinData` be the script context data.
```haskell
fromBuiltinData datum          ≡ (Just marloweData   :: Maybe MarloweData)
fromBuiltinData redeemer       ≡ (Just markloweInput :: Maybe MarloweInput)
fromBuiltinData scriptContext' ≡ (Just scriptContext :: Maybe ScriptContext)
```


#### *Constraint 2.* Single Marlowe script input

The output of exactly one Marlowe UTxO is spent in the transaction.
```haskell
filter (isMaroweAddress . txOutAddress . txInfoResolved) (txInfoInputs $ scriptContextTxInfo scriptContext) ≡ [TxInInfo _ (TxOut _ inValue _)]
  where isMarloweAddress (Address (ScriptCredential hash)) = hash == marloweValidatorHash
        isMarloweAddress _ = False
```
Hereafter we use `inValue` as the script value being spent.


#### *Constraint 3.* Single Marlowe output

There is a single output to the Marlowe script address unless the contract closes.
```haskell
txOutContract transactionOutput /= Close ⇒ filter (isMaroweAddress . txOutAddress) (txInfoOutputs $ scriptContextTxInfo scriptContext) ≡ [TxOut _ outValue (Just outDatum)]
  where isMarloweAddress (Address (ScriptCredential hash)) = hash == marloweValidatorHash
        isMarloweAddress _ = False
```
Hereafter we use `outValue` for the value being output to the script address and `outDatum` being the datum associated with that UTxO.


#### *Constraint 4.* No output to script on close

When a contract closes, the transaction does not output to the Marlowe script address.
```haskell
txOutContract transactionOutput == Close ⇒ filter (isMaroweAddress . txOutAddress) (txInfoOutputs $ scriptContextTxInfo scriptContext) ≡ []
  where isMarloweAddress (Address (ScriptCredential hash)) = hash == marloweValidatorHash
        isMarloweAddress _ = False
```


#### *Constraint 5.* Input value from script

The value of the Marlowe UTxO being spent exactly matches the value recorded in the Marlowe state.
```haskell
inValue ≡ foldMap toValue (accounts $ marloweState marloweData)
  where toValue ((_, Token currency name), count)) = Value.singleton currency name count
```


#### *Constraint 6.* Output value to script

The beginning balance plus the deposits equals the ending balance plus the payments.
```haskell
inValue + foldMap valueOfDeposit (fmap getInputContent transactionInput) ≡ outValue + foldMap valueOfPayment (txOutPayments transactionOutput)
  where getInputContent (Input inputContent) = inputContent
        getInputContent (MerkleizedTxInput inputContent _) = inputContent
        valueOfDeposit (IDeposit _ _ (Token currency name) count) = Value.singleton currency name count
        valueOfDeposit _ = mempty
        valueOfPayment (Payment _ (Party _) value)) = value
        valueOfPayment _ = mempty
```


#### *Constraint 7.* Input state

The input state of the transaction is used in the semantics computation.
```haskell
inState ≡ marloweState marloweData
```


#### *Constraint 8.* Input contract

The input contract of the transaction is used in the semantics computation.
```haskell
marloweContract marloweData ≡ inContract
```


#### *Constraint 9.* Marlowe parameters

The Marlowe parameters are not changed by the transaction.
```haskell
Just $ marloweParams marloweData ≡ fmap marloweParams . fromBuiltinData $ getDatum outDatum
```


#### *Constraint 10.* Output state

The output state of the transaction matches the result of the semantics computation.
```haskell
Just $ txOutState transactionOutput ≡ fmap marloweState . fromBuiltinData $ getDatum outDatum
```


#### *Constraint 11.* Output contract

The output contract of the transaction matches the result of the semantics computation.
```haskell
Just $ txOutContract transactionOutput ≡ fmap marloweContract . fromBuiltinData $ getDatum outDatum
```


#### *Constraint 12.* Merkleized continuations

All of the merkleized contracts referenced in the input are provided to the script.
```haskell
all demerkleizes transactionInput ≡ True
  where demerkleizes (MerkelizedTxInput _ hash) = isJust (fromBuiltinData =<< lookup hash (txInfoData $ scriptContextTxInfo scriptContext) :: Maybe Contract)
        demerkleizes (Input _) = True
```


#### *Constraint 13.* Positive balances

All accounts in the state have positive balances.
```haskell
all ((> 0) . snd) (accounts $ marloweState marloweData) ≡ True
```


#### *Constraint 14.* Inputs authorized

The required signature or at-least-one role token is present in the transaction for each deposit or choice.
```haskell
all authorized (fmap getInputContent transactionInput) ≡ True
  where getInputContent (Input inputContent) = inputContent
        getInputContent (MerkleizedTxInput inputContent _) = inputContent
        authorized (IDeposit _ (PK hash) _ _) = authorizedHash hash
        authorized (IDeposit _ (Role role) _) = authorizedRole role
        authorized (IChoice (ChoiceId _ (PK hash)) _) = authorizedHash hash
        authorized (IChoice (ChoiceId _ (Role role)) _) = authorizedRole role
        authorized INotify = True
        authorizedHash hash = elem hash (txInfoSignatories $ scriptContextTxInfo scriptContext)
        authorizedRole role = Value.leq (Value.singleton (rolesCurrency $ marloweparams marloweData) role 1) spent
        spent = foldMap (txOutValue . txInInfoResolved) (txInfoInputs $ scriptContextTxInfo scriptContext)
        -- FIXME: We should write out `Value.leq` ourselves, so the specification does not depend upon Plutus's convenience functions.
```


#### *Constraint 15.* Sufficient payment

Sufficient funds are paid to the public keys or roles.
```haskell
all sufficient (txOutPayments transactionOutput)
  where sufficient (Payment _ (Party (PK hash)) value) = Value.leq value (foldMap (paidToHash hash) (txInfoOutputs $ scriptContextTxInfo scriptContext))
                   (Payment _ (Party (Role role)) value) = any (paysToRole role value) (txInfoOutputs $ scriptContextTxInfo scriptContext)
        -- FIXME: Should `paysToRole` test the value in *any* UTxO or the *sum* of values?
        paysToRole (TxOut txOutAddress txOutValue (Just hash)) role value = txOutAddress == (Ledger.scriptHashAddress . rolePayoutValidatorHash $ marloweParams marloweData)
                                                                              && lookup hash (txInfoData $ scriptContextTxInfo scriptContext) == Just (Datum $ toBuiltinData role)
                                                                              && Value.leq value txOutValue
        paysToRole _ _ = False
        paidToHash hash (TxOut (Address (PubKeyCredential hash') _) txOutValue _) = if hash == hash' then txOutValue else mempty
        -- FIXME: We should write out `Value.leq` ourselves, so the specification does not depend upon Plutus's convenience functions.
```


## Plutus Validator for Marlowe Payouts


The Marlowe payout validator for roles is a Plutus script parameterized by currency symbol.
```haskell
rolePayoutValidator :: CurrencySymbol -> TokenName -> () -> ScriptContext -> Bool
```


### Specification

Consider the application of the Marlowe payout validator:
```haskell
validationResult' = rolePayoutValidator rolesCurrency' role () scriptContext
```

The validation fails (via returning `False` for `validationResult'` or via the throwing of an error) if any of the following constraints does not hold.


#### *Constraint 16.* Typed validation

The bytes for datum, redeemer, and script context deserialize to the correct types.

Let `datum :: BuiltinData` and `redeemer :: BuiltinData` be the datum and redeemer bytes in the script witness for spending the payout UTxO, and let `scriptContext' :: BuiltinData` be the script context bytes.
```haskell
fromBuiltinData datum          ≡ (Just role          :: Maybe TokenName)
fromBuiltinData redeemer       ≡ (Just ()            :: Maybe ())
fromBuiltinData scriptContext' ≡ (Just scriptContext :: Maybe ScriptContext)
```


#### *Constraint 17.* Payment authorized

The role token matching the datum must be spent in the transaction in order to redeem funds from the script.
```haskell
Value.leq (Value.singleton rolesCurrency' role 1) spent
  where spent = foldMap (txOutValue . txInInfoResolved) (txInfoInputs $ scriptContextTxInfo scriptContext)
        -- FIXME: We should write out `Value.leq` ourselves, so the specification does not depend upon Plutus's convenience functions.
```
