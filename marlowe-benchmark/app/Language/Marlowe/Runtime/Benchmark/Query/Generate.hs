-- | Benchmark for protocols.
module Language.Marlowe.Runtime.Benchmark.Query.Generate (
  -- * Benchmarking
  queries,
) where

import Language.Marlowe.Protocol.Query.Types (
  ContractFilter (ContractFilter),
  PayoutFilter (PayoutFilter),
  WithdrawalFilter (WithdrawalFilter),
 )
import Language.Marlowe.Runtime.Benchmark (BenchmarkConfig (..))
import Language.Marlowe.Runtime.Benchmark.Query (Query (..))
import Language.Marlowe.Runtime.ChainSync.Api (Address, AssetId (..), PolicyId, TxId, TxOutRef)
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweMetadataTag)

import qualified Data.Map.Strict as M (fromList, singleton)
import qualified Data.Set as S (Set, fromList, toList)

queries :: BenchmarkConfig
queries =
  BenchmarkConfig
    { headerSyncParallelism = 1 -- 4
    , headerMaxContracts = maxBound
    , bulkParallelism = 0 -- 4
    , bulkPageSize = 128
    , bulkMaxBlocks = maxBound
    , syncParallelism = 0 -- 4
    , syncBatchSize = 512
    , queryParallelism = 0 -- 4
    , queryBatchSize = 16
    , queryPageSize = 256
    , complexQueries =
        M.singleton "headers" headersQuery
          <> label "state" stateQuery
          <> label "transaction" transactionQuery
          <> label "transactions" transactionsQuery
          <> label "withdrawal" withdrawalQuery
          <> M.singleton "withdrawals" withdrawalsQuery
          <> M.singleton "payouts" (payoutsQuery Nothing)
          <> M.singleton "payouts pending" (payoutsQuery $ Just False)
          <> M.singleton "payouts withdrawn" (payoutsQuery $ Just True)
          <> label "payout" payoutQuery
    , lifecycleParallelism = 4
    , lifecycleContracts = 3
    }
  where
    label k vs = M.fromList $ zipWith ((,) . ((k <> " ") <>) . show) ([1 ..] :: [Int]) vs

partyAddresses :: S.Set Address
partyAddresses =
  S.fromList
    [ -- Four most frequent addresses on `mainnet`.
      "016992a73cadec1a81cc403b003d53ebe828e511888520712a69f59d00429ba68aa17fff5b336fcda9f41875ef8c89a6b1517acea1b1910a7e"
    , "017c0ae4b3fdc0f3ec7c440e1dd3807d699aaa8ef2b66b9b8101400412c3f6055df348bbdf443716d783e215a781c504a256c349ea6c72bea4"
    , "016f1a5edcc3538e2d343039647844a5e3dff5f5de20648530c357fd7c9397c4d4f5cd2d98ceebd14dd7ef47d988e928a94cf3cafe02e19bdc"
    , "0145c9be1a5de044940b788899ce8f31ec177163629f901da83150bd2ec3f6055df348bbdf443716d783e215a781c504a256c349ea6c72bea4"
    , -- Four most frequent addresses on `preprod`.
      "00b61bfb5ad5e54275b2ec6a16ff679d3937a6f8285f762905ec2dedd4b38d87aa7921ec407d89db9019f2fc0d9950a0ee3995fe8704237115"
    , "0087f1fbf8ef77813f51950ce495cd9ee962077f98b6dc70584ef02b89c3f6055df348bbdf443716d783e215a781c504a256c349ea6c72bea4"
    , "600a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07"
    , "60b80600589f0850ec775409e5cb98e276175ce814202f3350c4df97d5"
    , -- Four most frequent addresses on `preview`.
      "600a11b0c7e25dc5d9c63171bdf39d9741b901dc903e12b4e162348e07"
    , "007c0ae4b3fdc0f3ec7c440e1dd3807d699aaa8ef2b66b9b8101400412c3f6055df348bbdf443716d783e215a781c504a256c349ea6c72bea4"
    , "0055ff7536247204d8775d1310209e32899b51b9962619f01f711c22adc3f6055df348bbdf443716d783e215a781c504a256c349ea6c72bea4"
    , "000395fa118961ce2666d8eac1a1e5ae8ac8bd14290ddf3c83b19834e6c3f6055df348bbdf443716d783e215a781c504a256c349ea6c72bea4"
    ]

partyRoles :: S.Set AssetId
partyRoles =
  S.fromList
    [ -- Four most frequent roles on `mainnet`.
      AssetId "f0ff48bbb7bbe9d59a40f1ce90e9e9d0ff5002ec48f232b49ca0fb9a" "c.marlowe"
    , AssetId "f0ff48bbb7bbe9d59a40f1ce90e9e9d0ff5002ec48f232b49ca0fb9a" "e.cary"
    , AssetId "f0ff48bbb7bbe9d59a40f1ce90e9e9d0ff5002ec48f232b49ca0fb9a" "m.herbert"
    , AssetId "f0ff48bbb7bbe9d59a40f1ce90e9e9d0ff5002ec48f232b49ca0fb9a" "j.lumley"
    , -- Four most frequent roles on `preprod`.
      AssetId "8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d" "c.marlowe"
    , AssetId "8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d" "e.cary"
    , AssetId "8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d" "m.herbert"
    , AssetId "8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d" "f.beaumont"
    , -- Four most frequent roles on `preview`.
      AssetId "e6d1cfe947e4e2bd6063b1cf66bfaad2972c259e5160be904558f1c8" "SellerRole"
    , AssetId "e6d1cfe947e4e2bd6063b1cf66bfaad2972c259e5160be904558f1c8" "BuyerRole"
    , AssetId "e6d1cfe947e4e2bd6063b1cf66bfaad2972c259e5160be904558f1c8" "MediatorRole"
    , AssetId "10137ccc04dc177e9af17c7800f3fbe86855c63d22fad293b8d98695" "Party1"
    ]

roleCurrencies :: S.Set PolicyId
roleCurrencies =
  S.fromList
    [ -- Four most frequent roles currencies on `mainnet`.
      "f0ff48bbb7bbe9d59a40f1ce90e9e9d0ff5002ec48f232b49ca0fb9a"
    , "bd0dddf6d44a37ba142096f71d281159544378c4f17887a39797c002"
    , "a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235"
    , "346ec66c37f28ef21ee262eaf41dfe3605aa3344dd236a6003cb606e"
    , -- Four most frequent roles currencies on `preprod`.
      "8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d"
    , "774d4f5daedfb7734cc60b5f05a9c66647534d862ed2831e8bb486f9"
    , "19ef8d0e350bd0cf805374a0496a2f00afa7e3bf8574e966c891c919"
    , "fba787641d6b26f4af28ed0f5c1d0fb4d0a217ad7fb2e729cded9f98"
    , -- Four most frequent roles currencies on `preview`.
      "e6d1cfe947e4e2bd6063b1cf66bfaad2972c259e5160be904558f1c8"
    , "10137ccc04dc177e9af17c7800f3fbe86855c63d22fad293b8d98695"
    , "98f0b86e140afbde56cff758e50ae1e718cd833794ea073cabcc84ab"
    , "7137b60f84faaff1d1c02001a2483379270450edb6bae4715081525d"
    ]

tags :: S.Set MarloweMetadataTag
tags =
  S.fromList
    [ -- Four most frequent tags on `mainnet`.
      "run-lite"
    , -- Four most frequent tags on `preprod`.
      "5july2023"
    , "run-lite"
    , "run-lite-addr_test1qq6lgsnllsnjm6sff57d9926lmfs62mhlgepd9lhvad7vjv3wsm4klfud6a7a826d86seqmw86l7fdugvkfur5k48wzsn3udmf"
    , "marlowe.examples.vesting.v0.0.5"
    , -- Four most frequent tags on `preview`.
      "run-lite"
    , "dapp-test"
    , "initialised"
    , "buy-kevin-a-coffee"
    ]

contractIds :: S.Set ContractId
contractIds =
  S.fromList
    [ -- Four contracts with the most transactions on `mainnet`.
      "15b9ce2788f76dfd867d94abe8e5c9ec88f761cc0b54ca01c4ab31494938b352#1"
    , "abca28f50f06b2e4329cb1ae7e5068d199d14681990f1f161a0a121d12efee55#1"
    , "170fab972320d07ed1fdfb3f7460beffd6459eabda5453f1ce71085989fccc6d#1"
    , "6f13134c0d76c66ca23628cfef1b18bf26f05c7980998af926cb1673795b5dd2#1"
    , -- Four contracts with the most transactions on `preprod`.
      "d2755f7857089390f00e333d4ab04516a19545eac51206ac07719f38e4f88afa#1"
    , "4a4b39461c9bd74de546174e5425f7cfd03c97ed19a2e2631136f643f8936987#1"
    , "dfe1965a943d107a2bcc790ef602c6f841b8367877f79c1dba10eb198d736be6#1"
    , "a80e92fbda2f18e27fadecd399b41a18d279a321e8dd79a6fe95845fe09c4aa7#1"
    , -- Four contracts with the most transactions on `preview`.
      "8a4203fd5443cf18708d0d1b6b8a581006b9eaded2d84f7402f214bb86fb603e#1"
    , "1642777d3a83344b60b486a469e1924ea7229d5a246df83b5ab56d3dacafa92e#1"
    , "1c1dba0519ff9da9783a01a1456ae6425b6c5afda46568dc6a2a68300fc3cc62#1"
    , "12fbbf9b370c20721ba84652be85e0e34a8f80daea914bf025f6412aef6e1ed7#1"
    ]

transactionIds :: S.Set TxId
transactionIds =
  S.fromList
    [ -- Four transactions on `mainnet`.
      "0d5c3462fa4c798fa0deb3bcb7dd432e1f835c1baa3c452ed5d83886b474b6e0"
    , "eeab084afa3abc3ab8167784122cf2f0e536e656cad767bf95d98d13b6c2af08"
    , "00a0bc7fe3f6e2a11713119efb7dfff9f7aea9c5e7b37ee2c85e41c4618d68c3"
    , "14d7543fdf706a103dd4df55406a25ba43e99044f05f743408f199fefb6493ba"
    , -- Four transactions on `preprod`.
      "63499d5324ca07dce64af584f2726e5ce2b54ec46eafabd82d79d55d6edc6172"
    , "b75608da7708a4d7cb1b9a601e517e45bc5ee6e2cec2ccff7393833f58ad2f99"
    , "4a9948200984d0a71cad1415e671c939aa0ac5594d1ad7953037496a7d2a015c"
    , "d0ab997cc6fed7642deb8070a6570e7d3b190caf97f04f5f4460ee09d7b38401"
    , -- Four transactions on `preview`.
      "ccd54b3f598c090cd0e01b96dfeafdf108883ea91f0caa0af68240b5710e1df1"
    , "62bcb92210d470e457d1dbf2037a0b7579bf3803eb39a22ef7766e3b4fa7b40f"
    , "89f089ea0daa11fd4c1c7105de926067a4a6b72f8825d0fe1320da2cf266b23c"
    , "1af114f2b5c198e0167c0610ed6746cfc29e335f5f9c842f8ae6f96126492565"
    ]

withdrawalTxIds :: S.Set TxId
withdrawalTxIds =
  S.fromList
    [ -- Four withdrawal transactions with the most payouts on `mainnet`.
      "17c121df1218788fd478f5ca225b7343f20d0fc3a79d48f90ef23ae8461460e8"
    , "8f56d4b0a9bb690d181953578289d298a6b03a2574a7edaad092dd34d5f18bf9"
    , "55b599cc47dcea2ef923a250d153d09b362cc4252a1dc37d0b40cde232a0d5c1"
    , "00db2cdb08a338bcab7ddef47de84469ba7ef8dfa0d6833e0a915e17ecd756d9"
    , -- Four withdrawal transactions with the most payouts on `preprod`.
      "ef7ff312132a5c1dcc5bffe054802eca1323b96d7da2e9017d0eaf25ada32dff"
    , "d7bb310a9961a8b8eae32666d4db241e63798c252486228c62fa1133b0c44465"
    , "8f655e848f76d9558fe4fcf0f3985df266b3c0f6a2bb0d890bae92dc276e7286"
    , "6b56da0f15de304077bcdc1b2f0381df24bd3db39daa2ec68ff240b8b04d228c"
    , -- Four withdrawal transactions with the most payouts on `preview`.
      "1301080993a990d2c2e0c7145b0d0a82fe4eb7fa10c9beece7d1f756f0cf55cf"
    , "70ef551d6f087f6b477a07d904ba3a712d15a57b1a837d777989e12069b88c47"
    , "e4136e25c65b77fed574fb29114ec499bbb61a314b31931dffd8d7c64a172164"
    , "fb8a113a645a566902972025f7f165fc53ac74a7399d40c29c09a5a4246effc2"
    ]

payoutTxOutRefs :: S.Set TxOutRef
payoutTxOutRefs =
  S.fromList
    [ -- Four payouts on `mainnet`
      "dcde5e04d5a9077276f4128fcb307953accd6b510a5a63313e652e5d63bd3b06#2"
    , "7275f02ba1245ec89a12b2221406b6d5d38b3a7bfe74b5536bb242cc6ac944a0#3"
    , "6e41846908175fbbea9a5c808901c5e1a1b790c1f899faa1c5c6bb887b5456e2#1"
    , "c1246f0f0d47804be9ee4c0439065c438a5c03c852781a1c3e390a0683697885#2"
    , -- Four payouts on `preprod`
      "356f97b58570081a2316661a48c2620504f0d7b59926dc091319b71490b17d88#2"
    , "5a2ed147be32cc79cb147a29b1af4ac863449b6f10c1762c4e2cec9cee454394#3"
    , "e00648cf9ed9392ad160b1c93ecf325d51bf8c523841b6fe192c9bee3d4d61c0#2"
    , "b1d213bc6e42e967239c20b29f8c69706b55e2d8460a9e0c1b3b08ab6e029a96#2"
    , -- Four payouts on `preprod`
      "1af114f2b5c198e0167c0610ed6746cfc29e335f5f9c842f8ae6f96126492565#2"
    , "ae3503a21819f2f0e6a70ae332dafff3ee6944982a54b871f6ab1a389a0fd4ea#3"
    , "6fed19171509d03dd6ffc2878321d1aa93ac5c648cb50c001d51993506bdc0d7#1"
    , "89c92189ac6c1313af3d2e9d0a53ada200f1c1a372fe99c56ac9f635805514ac#3"
    ]

headersQuery :: Query
headersQuery = QueryHeaders $ ContractFilter tags roleCurrencies partyRoles partyAddresses

stateQuery :: [Query]
stateQuery = QueryState <$> S.toList contractIds

transactionQuery :: [Query]
transactionQuery = QueryTransaction <$> S.toList transactionIds

transactionsQuery :: [Query]
transactionsQuery = QueryTransactions <$> S.toList contractIds

withdrawalQuery :: [Query]
withdrawalQuery = QueryWithdrawal <$> S.toList withdrawalTxIds

withdrawalsQuery :: Query
withdrawalsQuery = QueryWithdrawals $ WithdrawalFilter roleCurrencies

payoutsQuery :: Maybe Bool -> Query
payoutsQuery w = QueryPayouts $ PayoutFilter w contractIds partyRoles

payoutQuery :: [Query]
payoutQuery = QueryPayout <$> S.toList payoutTxOutRefs
