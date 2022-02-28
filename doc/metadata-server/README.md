## Abstract

This document summarizes requirements and proposes a draft of a protocol for metadata information exchange for the on-chain transactions (`Tx`) which execute Marlowe contracts. The off-chain metadata storage mechanism does not rely on a centralized registry but provides schema which can be implemented by independent metadata providers.

## Motivation

Marlowe is as a high level level language with a set of properties which are crucial for writing reliable financial contracts. A possibility to audit the code of the contract which was already initiated on-chain seems to be critical to any participant "invited" to it. Marlowe (and its current implementation on chain) gives us access to the source code of the contract, ability to do static analysis over it and "step by step" simulation so it provides the fundamental layer of trust for all users.
To build a contract exchange platform and execution environment we want also to be able to associate and provide contract information suitable for human consumption (like title, description, statistics and certification etc.). We want to verify that provided metadata informations about the contract on chain are consistent with the contract content itself. Additionally we should allow the contract initiatior to attach additional set of data (like documents) to the particular contract instance and present it as well to all other participants. We should provide a way to verify that given custom metadata are also dedicated for the running on-chain contract.

We want to implement this in an efficient manner avoiding huge payloads on the blockchain and by use of existing or emerging Cardano standards. Additionally we need to keep the protocol decentralized so anybody can take a role of a metadata provider.

### Scope limits

We can identify additional sources of metadata informations which can be associated with contract transactions (`Tx`) running on the blockchain. There are also parts of proposed metadata format which we should extend in the future. These parts/problems we want to tackle in a separate proposals:

- Information associated with role tokens (icon, currency description etc.) which are presented in wallets and blockchain explorers.

- Identity verification of contract participants and exchange of information related with these identities (nicknames, credibility, certificates etc.).

- Metadata format and enhancement:

  - Localization,

  - Roles / choices identifiers in the metadata layer so we can complately separate presentation from the contract code,

- Encryption layer. We assume now that exchanged data are public or that the access to the information is restricted by a separate layer of the metadata server. In the future we want to probably propose some encryption schema as a part of the protocol.

## Verifing Marlowe Contract Metadata on the chain

### Terminology

- Marlowe contract (`Contract`) - a particular contract expression which can be executed on chain which have all particiapants and values (like amounts, timeouts etc.) set.

- Marlowe contract transaction (`MarloweTx`)- transaction on the block chain which is a part of the ongoing Marlowe contract.

- Marlowe contract metadata (`ContractMetadata`) - a particular set of information which we want to attach to a particular Marlowe contract (description, documents / agreements etc.)

- Transaction metadata (`TxMetadata`) - on chain information (`CBOR` encoded- [spec](https://github.com/input-output-hk/cardano-node/blob/master/doc/reference/tx-metadata.md)) which is attached to the transaction . It is inaccesible to the transaction validators and minting policy scripts.

- Marlowe contract transaction metadata (`MarloweTxMetadata`) - metadata attached to the on chain transaction executing Marlowe Contract (`MarloweTx`).

### `Contract` extraction

When initiator submits the `Contract` to the blockchain it builds up a trasaction which outputs define role tokens distribution alongside with the "contract continuation" output which is guarded by `Marlowe` interpreter script. Role tokens outputs are assigned to particular addresses chosen by the initiator - we can interpret them as invitations to the contract or permissions to be a contract party (this tokens shows up in the user wallet).
Because continuation output from the initial `Tx` contains the full code (or `Merkleized` version of it) of the `Contract` we are able to [extract it](https://github.com/input-output-hk/marlowe-cardano/blob/f452d98942a14d34c10f2963ec4b84147da5621c/marlowe/src/Language/Marlowe/Client/History.hs). In other words when someone is "invited" to the already initiated on chain contract we are able to extract the Marlowe code from that transaction and present it to the possible participant for an audit, simulation etc. We can do this at any stage of the contract because we can track back to the initial transaction and perform the extraction.

<!-- [ IMAGE -> Extraction of the contract -> Simulation / Audit ] -->

### `MarloweTxMetadata`

The submitter of the transaction have an ability to attach additional (`CBOR` encoded) [data to it](https://github.com/input-output-hk/cardano-node/blob/master/doc/reference/tx-metadata.md). This data are not accessible to the transaction validators so they can be arbirary informations not necessarly connected and valid to the `Tx` real content. To simplify the disussion for now and assume that there are no size limits and costs associated with `TxMetadata` and we use this layer to store all contract metadata.

FIXME:
How do we detect which transactions / tokens are the marlowe ones.

FIXME (move / fix this):
It is not trivial currently to identify which transactions are Marlowe contracts currently but we can use `TxMetadata` layer for this purpose too.
The real limitiations and possible usage scenarios are discussed in the [Implemeantation](# implementation) part.

### `MarloweTxMetadata` verification

We can imagine that we want to endorse a particular contract with a set of certified documents (agreements, audits etc.). We don't want to necessarily trust the user who creates the `MarloweTx` so we should assume that the contract code can be incosistent with `TxMetatdata` which provide the documents data. The simple protocol could require metadata to be signed together with the Marlowe contract so the connection between those pieces is clear.

<!-- IMAGE -> Extraction of the contract -> Extraction of the metadata -->

Because `Marlowe` abstracts over addresses using roles the full meaning of a particular instance of a contract is also dependent "role distribution". It seems that semantic meaning of a `Contract` on chain is fully defined by triple:
(`Contract`, role distribution, `MarloweValidator`). Using a pseudo code and removing a lot of implementation details we can provide trivial validation of `MarloweTxMetadata`:

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

> In the current implemenatation we need also `MarloweParams` to derive the final validator but we can treat this pieces as an implementation detail. It the context of [CIP-33](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0033) it seems beneficial to move `MarloweParams` to datum so the validator script could be shared .

It may seem a bit articial or unrealistic that we want to delegate submition process to some third parties and not rely on the submitter of the `MarloweTx` as the author of metadata attachments. On the other hand there are some cases where this separation seems to be useful:

- Marlowe Marketplace can possibly perform submitions of paid and certified contracts on behalve of the user. We may want to create this transactions ourselves so we can confirm to others that we really give a credit to a given transaction and we associate it with certified contract template. User should pass to us contract parameters together with signed `MarloweTxMetadata` (which contains the required triple) so we can still proove to others that a given `MarloweTx` was created and described by a particular party.
  Of course we are not able to prevent users to use certified / paid templates on their own (without paying fee) but in such a case we are not going to endorse these transactions on chain.

- In multiparty agreement it can be not so important who is the owner of the initial transaction (contract participant, some third party auditor, layer etc.). On the other hand it is rather important that the attachment for a transaction was really dedicated for its semantic meaning.

- Using these schema we can require distribution of role tokens to a specific, generic utility scripts with constant addresses (like "deposit scripts") and provide the same audited Marlowe contract with the same role distribution which can be safely reused and certified. By using this schema we can push a possible role distribution logic outside of our Marlowe interpreter / validator (we don't have to change our implementation) to role scripts (with single and simpler to audity responsiblity) and distribute reusable instances of Marlowe contracts together with verified set of metadata (authorized for this Conract and hard coded role to scripts distribution).

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

In the current scenario we want to verify validity of two parts of `ContractMetadata` (`(InsMetadata, ExtMetadata)`) to do this we can slightly modify the previous verification schema. `InsMetadata` can be verified using the same strategy as before because it is information dedicated for a specific Contract so we need to prove the connection of the metadata with onchain triple: roles distribution, `Validator` and `Contract`.

When we want to verify the `ExtContract` and `ExtMetadata` are really associated with the data on chain we should prove that there is a set of parameters which when applied to `ExtContract` produced the same `Contract` value as we have in this particular `Tx`. The validation of `ExtContract` and `ExtMetadata` association should be provided by a seperate certificate.

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

`transaction_metadatum_label` is a namespace which dApps should use to indicate the type and purpopse of the transaction and its metadata. To properly use this standard we should reserve a place in an [existing label registry](https://github.com/cardano-foundation/CIPs/blob/master/CIP-0010/registry.json) which is managed by Cardano Foundation. Let's assume that we use a single namespace `1564` (Kit Marlowe [1564-1593]) for Marlowe metadata exchange.

### `transaction_metadatum`

`transaction_metadatatum` can be nearly any `CBOR` encoded data (with string / byte buffers length limitations to `64B`). [Tx metadata specification](https://github.com/input-output-hk/cardano-node/blob/master/doc/reference/tx-metadata.md#metadata-schemas---mappings-and-formats) defines a way how to encode metadata using `cardano-cli` and `JSON` encoded payload. For the purpose of this draft we use simplified ["no schema"](https://github.com/input-output-hk/cardano-node/blob/master/doc/reference/tx-metadata.md#no-schema) style spec (just plain `JSON`) of the format which is more readble and can be quickly tested with `cardano-cli`.
We should expand this in the next iterations into a [detailed schema](https://github.com/input-output-hk/cardano-node/blob/master/doc/reference/tx-metadata.md#detailed-schema) which is translated into a precise CBOR representation (both `cardano-cli` and [`node-wallet`](https://input-output-hk.github.io/cardano-wallet/user-guide/common-use-cases/TxMetadata) support it). We want to use precise CBOR encoding because this exchange protocol should be tool and language agnostic (reading data from the chain.

#### Storage

Using blockchain as the only source of data has many advantages - we don't have to rely on things like hosting, domain registration, maintenance of the data storage systems etc. On the other hand storing data on chain has its cost because it increases transaction fee. `TxMetadata` size contributes to the overall transaction size (without any additional fees). Additionally transactions have currently [64KB](https://github.com/cardano-foundation/CIPs/blob/master/CIP-0009/README.md#updatable-protocol-parameters) size limit which is additional "risky" factor.

We try to support both approaches of metadata distribution i.e. on-chain and off-chain by using optional keys in the schema. Of course on chain metadata are immutable and we don't allow now updates of them on the chain (i.e. [NFT metadata in theory supports updates](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0025#update-metadata-link-for-a-specific-token)). Instead we allow usage of both versions and off-chain provider (if responsive) can be used for metadata updates.

### Draft of the metadata schema

Following [CIP-0026 convention](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0026):

- We use [_Blake2b-256_](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0026#hash-function),

- We use [this encoding](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0026#attestation-signatures) constructing messages for signing.

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



