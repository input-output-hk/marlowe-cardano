## Abstract

This document summarizes requirements and proposes a draft of a protocol for metadata information exchange for the on-chain transactions (`Tx`) which executes Marlowe contracts. The off-chain metadata storage mechanism does not rely on a centralized registry but provides schema which can be implemented by independent metadata providers.

## Motivation

Marlowe is a high-level language with a set of properties which are crucial for writing reliable financial contracts. A possibility to audit the code of the contract which was already initiated on-chain seems to be critical to any participant "invited" to it. Marlowe and its current implementation on Cardano chain gives us access to the source code of the contract, ability to do static analysis over it and "step by step" simulation so it provides the fundamental layer of trust for the language users.

To build a contract exchange platform (Marlowe Marketplace) and execution environment (Marlowe Run) we want to be able to also associate and provide contract template information suitable for human consumption (like title, description, statistics and certification etc.). We want to verify that provided metadata information about the transaction template is consistent with the Marlowe contract embedded in the transaction on-chain. Additionally we should allow the contract initiator to attach a specific set of data (like documents, agreements) to the particular contract instance. We want to be able to provide a validity proof for these data and present it to all other participants as well if necessary.

We want to implement this in an efficient manner avoiding huge payloads on the blockchain and by use of existing or emerging Cardano standards. Additionally we need to keep the protocol decentralized so anybody can take the role of a metadata provider.

### Scope limits

We can identify additional sources of metadata information which can be associated with contract transactions running on the blockchain. There are also parts of the proposed metadata format which we should extend in the future. These parts/problems we want to tackle in a separate proposals:

- Role token metadata (icon, currency description etc.) handling. They are presented in Cardano wallets and blockchain explorers.

- Localization (`l10n`) layer for the metadata format.

- Separation of roles / choices identifiers from the presentation layer. These identifiers should have representation in the metadata part as well.

- Encryption layer. We assume now that exchanged data are public or that the access to the information is restricted by a separate layer of the metadata server. In the future we want to probably propose some encryption schema as a part of the protocol.

- Proposal for "identity layer" using Atala PRISM. With identities we can associate and exchange some metadata information like nicknames, credibility etc.

## Verifying Marlowe Contract Metadata on the chain

### Terminology

- Marlowe contract (`Contract`) - a particular contract expression which can be executed on-chain which has all participants and values (like amounts, timeouts etc.) set.

- Marlowe contract transaction (`MarloweTx`)- transaction on the block chain which is a part of the ongoing Marlowe contract.

- Marlowe contract metadata (`ContractMetadata`) - a particular set of information which we want to attach to a Marlowe contract (description, documents / agreements etc.)

- Role distribution (`RoleDistribution`) - when contract uses roles as `Party` values we use NFTs to represent roles on-chain. The initial transaction pay these NFTs to some addresses so it defines this initial distribution.

- Transaction metadata (`TxMetadata`) - on-chain information ([`CBOR` encoded](https://github.com/input-output-hk/cardano-node/blob/master/doc/reference/tx-metadata.md)) which is attached to the transaction . It is inaccessible to the transaction validators and minting policy scripts.

- Marlowe contract transaction metadata (`MarloweTxMetadata`) - metadata attached to the on-chain transaction executing Marlowe Contract (`MarloweTx`) related to the `Contract`.


### `Contract` extraction

When initiator submits a `Contract` to the blockchain it builds up a transaction which outputs define role tokens distribution. Additionally transaction contains "continuation" output which is "guarded" by `Marlowe` interpreter script. Role tokens outputs are assigned to a set of addresses chosen by the `Tx` author. We can interpret them as invitations to the contract or permissions to be the contract party (this tokens shows up in the user wallets).

Because continuation output from the initial `Tx` contains the full code ("modulo" `Merkleization`) of the `Contract` we are able to [extract it](https://github.com/input-output-hk/marlowe-cardano/blob/f452d98942a14d34c10f2963ec4b84147da5621c/marlowe/src/Language/Marlowe/Client/History.hs). In other words when someone is "invited" to the already initiated on-chain contract we are able to extract the Marlowe code from that transaction and present it to the possible participant for an audit, simulation etc. before he / she commits to it. We can do this at any stage of the contract progression because we can track the chain back to the initial transaction and perform the extraction.

<!-- [ IMAGE -> Extraction of the contract -> Simulation / Audit ] -->

### `MarloweTxMetadata`

The submitter of the transaction have an ability to attach additional [`CBOR` encoded data to it](https://github.com/input-output-hk/cardano-node/blob/master/doc/reference/tx-metadata.md). This data are not accessible to the transaction validators so they can be arbitrary information not necessarily valid and related to the `Tx` content. To simplify the discussion for now and assume that there are no size limits and costs associated with `TxMetadata` and we use this layer to store all contract metadata.

### `MarloweTxMetadata` verification

We can imagine that we want to endorse a particular contract with a set of certified documents (agreements, audits etc.). We don't want to necessarily trust the user who creates the `MarloweTx` so we should assume that the contract code can be incosistent with `TxMetatdata` which provides the documents data. The simple protocol could require metadata to be signed together with the Marlowe contract so the connection between those pieces is clear.

<!-- IMAGE -> Extraction of the contract -> Extraction of the metadata -->

Because `Marlowe` abstracts over addresses using roles the full meaning of a particular instance of a contract is also dependent on role distribution. It seems that semantic meaning of a `Contract` on-chain is fully defined by the triple: `(Contract, RoleDistribution, MarloweValidator)`. Using a pseudo code we can provide this basic validation flow for `MarloweTxMetadata`:

```purescript
type RoleDistribution = Map Role PubKeyHash

type MarloweTxMetadata =
  { signature :: Signature
  , payload ::
    { metadata :: MarloweContractMetatada
    , contract :: Contract
    , roleDistribution :: RoleDistribution
    }
  }

extractMetadata :: OnChainTx -> Maybe Metadata
extractMetadata tx = do
  marloweTxMetadata <- decode <$> extractMetadata tx
  txContract <- extractMarloweContract tx
  txValidator <- extractValidator tx
  txRoleDistribution <- extractRoleDistribution tx

  if txMetadata.payload.contract == txContract
    && txMetadata.payload.roleDistribution == txRoleDistribution
    && checkValidator txValidator
    && checkSignature txMetadata.payload pubKey MarloweTxMetadata.signature
      then
        Just MarloweTxMetadata.metadata
      else
        Nothing
```

> In the current implementation `MarloweParams` are used to derive the final validator. `MarloweParams` provide "low level" details (the currency symbol) which are used to implement roles handling on chain. From the perspective of the contract semantics they are not important. We treat them as implementation detail here hidden behind `checkValidator` but in the final implementation spec we should also handle them somehow.
> It is worth to mention that in the context of [CIP-33](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0033) it seems beneficial to move `MarloweParams` to the `datum` so the validator script could be shared and the transaction size could be minimized.

It may seem a bit artificial or unrealistic that we want to delegate submission process to some third parties and not rely on the submitter of the `MarloweTx` as the author of metadata attachments. On the other hand there are some cases where this separation seems to be useful:

- Marlowe Marketplace can possibly perform submissions of paid and certified contracts on behalve of the user. We may want to perform creation of these transactions by the platform so we can confirm to others that we really give a credit to a given transaction and we associate it with certified contract template. User should pass to us contract parameters together with signed `MarloweTxMetadata` (which contains the required triple) so we can still proove to the others that a given `MarloweTx` was created and described by a particular party.

  Of course we are not able to prevent users from using certified / paid templates on their own (without paying fee) because in some sens all Marlowe contracts are open sourced (at least through inspection of the `Tx`). In such a case we are not going to endorse these transactions on-chain and on the platform.

- In multiparty agreement it can be not so important who is the owner of the initial transaction (contract participant, some third party auditor, layer etc.). On the other hand it is rather important that the attachment for a transaction was really dedicated for its semantic meaning.

- Using these schema we can require distribution of role tokens to a specific, generic utility scripts with constant addresses (like "deposit scripts") and provide the same audited Marlowe contract with the same role distribution which can be safely reused and certified. By using this schema we can push a possible role distribution logic outside of our Marlowe interpreter / validator (we don't have to change our implementation) to role scripts (with single and simpler to audit responsiblity) and distribute reusable instances of Marlowe contracts together with verified set of metadata (authorized for this Contract and hard coded role to scripts distribution).

## Extended Marlowe

### Terminology

This is analogus terminology which we have in our codebase:

- Extended Marlowe contract (`ExtContract`) - marlowe contract which contains possible placeholders for values. We use it to parametrize amounts but in these parameters can use all value expressions. Marlowe playground allows us to build these kind of contracts.

- Extended contract instance (`Instance`) - extended contract which has all parameters specified / filled in. In essence it is just a proper Marlowe contract but we can associate it back with some contract template - we can point to its "source" and possibly authors, descriptions, parameters etc.

- Extended contract metadata (`ExtMetadata`) - metadata suitable for human consumtion about a particular extended contract (name, description, choice, role information etc.). We don't have an official schema definion for it but only a reference implementation. The schema / spec would be probably required if we want to allow other implementations / instances of metadata servers.

- Contract template (`Template`) - `ExtContract` together with its `ExtMetadata`. This would be probably the main type of artifact which we are going to distribute through Marketplace and which we want to certify so code and its human readble description are verified together.

- Instance metadata (`InsMetadata`) - part of the metadata dedicated related only to the `Instance`. For example we can imagine that the user picks a `ExtContract` from the `Marketplace` and in `Marlowe Run` he is be able to add its own information set to this particular instance (add own description, describe role differently, change title etc.). Parts of this metadata set could be probably complately service dependent (for example in different instances / deployments of `Marlowe Run` we can use different schema here and allow attachment of extra documents).

- Marlowe contract metadata (`ContractMetadata`) - the same as in the previous section but now we can look at it as a product: `(InsMetadata, ExtMetadata)`.

# Verification of `ContractMetadata`

In the current scenario we want to verify the validity of two parts of `ContractMetadata` (`(InsMetadata, ExtMetadata)`). To do this we can slightly modify the previous verification schema. `InsMetadata` can be verified using the same strategy as before because it is information dedicated for a specific Contract so we need to prove the connection of the metadata with onchain triple: roles distribution, `Validator` and `Contract`.

When we want to verify the `ExtContract` and `ExtMetadata` are really associated with the data on-chain we should prove that there is a set of parameters which when applied to `ExtContract` produced the same `Contract` value as we have in this particular `Tx`. `ExtContract` and `ExtMetadata` association should be proved by a separate signature / certificate.

<!-- IMAGE Extended contract comparision flow. -->

```purescript
type RoleDistribution = Map Role PubKeyHash

type Params = Map Id Marlowe.Semantics.Value

type MarloweTxMetadata =
  { instance ::
    { -- | Here we have the same set of data as before
    }
  , template ::
    { extContract :: ExtContract
    , params :: Params
    , metadata :: ExtMetadata
    -- | Prove that `metadata` set is dedicated for the
    -- | above `extContract` value.
    , signature :: Signature
    }
  }

type Metadata = { instance :: ..., template :: ... }

extractMetadata :: OnChainTx -> Maybe Metadata
extractMetadata tx = do
  marloweTxMetadata <- decode <$> extractMetadata tx
  txContract <- extractMarloweContract tx

  let
    templateTxMetadata = marloweTxMetadata.template

  extMetadata <- do
    if fillTemplate templateTxMetadata.extContract templateTxMetadata.params == txContract
      && checkValidator txValidator
      && checkSignature (templateTxMetadata.extContract /\ templateTxMetadata.metadata) MarloweTxMetadata.signature
        then
          Just templateTxMetadata.metadata
        else
          Nothing

  -- | For the `InsMetadata` we reuse a similar procedure
  -- | as in previous chapter
  insMetadata <- extractInstanceMetadata

  pure { insMetadata, extMetadata }
```

## Protocol

### Objectives

- We want to avoid centralization of metadata registry.

- We want to minimize blockchain resource usage in the default setup.

- We want to use [existing or emerging Cardano standards](https://cips.cardano.org/) and tools.

- Metadata format schema should be well defined but we should leave it open / extensible.

### Marlowe metadatum label

As a baseline channel for metadata exchange we want to use [`TxMetadata`](https://github.com/input-output-hk/cardano-node/blob/master/doc/reference/tx-metadata.md) which simplified schema looks like this:

```CDDL
transaction_metadata =
  { * transaction_metadatum_label => transaction_metadatum }
```

`transaction_metadatum_label` is a namespace which dApps should use to indicate the type and purpose of the transaction and its metadata. To properly use this standard we should reserve a place in an [existing label registry](https://github.com/cardano-foundation/CIPs/blob/master/CIP-0010/registry.json) which is managed by Cardano Foundation. Let's assume that we use a single namespace `1564` (Kit Marlowe [1564-1593]) for Marlowe metadata exchange.

### `transaction_metadatum`

`transaction_metadatatum` can be nearly any `CBOR` encoded data (with string / byte buffers length limitations to `64B`). [Tx metadata specification](https://github.com/input-output-hk/cardano-node/blob/master/doc/reference/tx-metadata.md#metadata-schemas---mappings-and-formats) defines a way to encode metadata using `cardano-cli` and `JSON` encoded payload. For the purpose of this draft we use simplified ["no schema"](https://github.com/input-output-hk/cardano-node/blob/master/doc/reference/tx-metadata.md#no-schema) style spec (just plain `JSON`) of the format which is more readble and can be quickly tested with `cardano-cli`.
We should expand this in the next iterations into a [detailed schema](https://github.com/input-output-hk/cardano-node/blob/master/doc/reference/tx-metadata.md#detailed-schema) which is translated into a precise CBOR representation (both `cardano-cli` and [`node-wallet`](https://input-output-hk.github.io/cardano-wallet/user-guide/common-use-cases/TxMetadata) support it). We want to use precise CBOR encoding because this exchange protocol should be tool and language agnostic.

#### Storage

Using blockchain as the only source of data has many advantages - we don't have to rely on things like hosting, domain registration, maintenance of the data storage systems etc. On the other hand, storing data on-chain has its cost because it increases transaction fees. `TxMetadata` size contributes to the overall transaction size (without any additional fees). Additionally transactions have currently [64KB](https://github.com/cardano-foundation/CIPs/blob/master/CIP-0009/README.md#updatable-protocol-parameters) size limit which is an additional "risky" factor.

We try to support both approaches of metadata distribution i.e. on-chain and off-chain by using optional keys in the schema. Of course on-chain metadata are immutable and we don't provide update mechanismfor them (i.e. [NFT metadata in theory supports updates](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0025#update-metadata-link-for-a-specific-token)). Instead we allow usage of both versions and off-chain metadata source (if responsive) can be used for metadata updates.

### Draft of the metadata schema

Following [CIP-0026 convention](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0026):

- We use [_Blake2b-256_](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0026#hash-function),

- We use [this encoding](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0026#attestation-signatures) to construct messages for signing.

#### Instance metadata

The `id` of the metadata can be derived from the transaction:

```js
contract = serialize(Contract)

roleDistribution = [{ "name": <string>, "pubKeyHash": <string> }]

insId = hash(
  hash(CBOR(contract)) +
  hash(CBOR(roleDistribution)) +
  validatorHash
)
```

`serialize(Contract)` - `JSON` serialized version of the Marlowe expression from the `Tx`
`validatorHash` - a marlowe validator hash bytestring
`CBOR` - [function](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0026#attestation-signatures) which encodes its `JSON` input into binary

We can define now the suggested shape of the `instanceMetadata`:

```js
insMetadata = {
  metadata = {
    shortDescription: <string>,
    longDescription: <string>,
    choiceDescriptions: [{ id: <string>, description: <string> }],
    roleDescriptions: [{ id: <string>, description: <string> }],
    <other properties>
  },
  "signatures": <string | array>
}
```

All properties of `metadata` value are optional but we require that this map is not empty. `signatures` are optional too but if they are present we consider them as [adequate](# verification-of-contract-metadata) to the transaction if they sign data together with its `insId`:

```js
signature = sign(hash(insId + CBOR(metadata)))
```

#### Template metadata

```js
extContractId = hash(CBOR(serialize(extContract)))

extMetadata = {
  template = {
    contract: <extContract>,
    metadata: {
      shortDescription: <string>,
      longDescription: <string>,
      choiceDescriptions: [{ id: <string>, description: <string> }],
      roleDescriptions: [{ id: <string>, description: <string> }],
      valueDescriptions: [{ id: <string>, description: <string> }],

      <other properties>
    }
  },
  "params": [{ id: <string>, description: <string> }]
  "signatures": <string | array>
}
```
`signatures` and `params` are optional as well as `template.contract`. `template.metadata` should not be empty. We consider signature to be relevant to the transaction when:

```js
signature = sign(hash(extContractId + CBOR(extMetadata.template.metadata)))
```

and

```js
txContract == fillTemplate(extMetadata.template.contract, extMetadata.params)
```

#### Tx metadata schema

```js
contractMetadata = {"instance": <insMetadata>, "extended": <extMetadata>}

txMetadata = {
  "1564": {
      "on": <contractMetadata>
      "off": {
        url: <string>
        subject: <string>
      }
    }
  }
```

- `subject` - optional value. By default `insId` should be used.
- `url` - metadata server which provides possibly two entries:
  - `/metadata/{subject}/property/instance`
  - `/metadata/{subject}/property/extended`

If both `on` and `off` parts are defined we treat `on` values as fallback ([`sequnenceNumber = 0`](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0026#sequence-numbers)) and prioritize responses from the server. We allow servers to respond with local redirects so entries can be lifted to subjects or even different REST resources i.e. `/metadata/{subject}/property/exteneded` could possibly redirect to `/extended/{extContractId}/`.

## Implementation

### Prototyping metadata server

For an initial protyping phase of the metadata server we can use libraries from (`input-output-hk/offchain-metadata-tools`)[`https://github.com/input-output-hk/offchain-metadata-tools`]. This project provides a basic server and simple database layer (based on `postgresql`) for key-value service which implements [CIP-0026 - Cardano Off-Chain Metadata](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0026).
In the case of Marketplace we want to probably expose only metadata subjects for the instances of contracts (like `/metadata/{ insId }/`) and expose templates and their metadata using other routing patterns (like `/extended/{ extContractId }/`) and use redirects for properties (`/metadata/{ insId }/property/extended`). Of course if we want to flatten both resources into the metadata subjects we can do this by using some encoding for inheritance on the `SQL` level but this comes with its own overhead and makes typing and schema of the API more complicated.

### `TxMetadata`

In the current implementation we use `PAB` to submit a transaction to the blockchain. It seems like support for the metadatadata handling is not implemented [by this layer](https://github.com/input-output-hk/plutus-apps/issues/93#issuecomment-1055562155).

