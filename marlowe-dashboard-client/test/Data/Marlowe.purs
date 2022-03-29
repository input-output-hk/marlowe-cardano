{-
   Test data factory for Marlowe types.
-}
module Test.Data.Marlowe where

import Prologue

import Control.Monad.Error.Class
  ( class MonadError
  , class MonadThrow
  , throwError
  )
import Control.Monad.Now (class MonadTime, now)
import Crypto.Encoding.BIP39.English as English
import Data.Address (Address(..))
import Data.Address.Bech32 (Bech32Address(..))
import Data.Address.Bech32.DataPart as BDP
import Data.Address.Bech32.DataPart.CodePoint (toCodePoint)
import Data.Argonaut (Json, encodeJson)
import Data.Array (length, (!!))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.BigInt.Argonaut as BigInt
import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Either (either)
import Data.Enum (class BoundedEnum, Cardinality(..), cardinality, toEnum)
import Data.Foldable (class Foldable, fold)
import Data.Int (hexadecimal, toStringAs)
import Data.List.Lazy (replicateM)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (maybe)
import Data.MnemonicPhrase (MnemonicPhrase)
import Data.MnemonicPhrase as MP
import Data.MnemonicPhrase.Word as Word
import Data.Newtype (over2)
import Data.Passphrase (fixmeAllowPassPerWallet)
import Data.PaymentPubKeyHash as PPKH
import Data.PubKeyHash (PubKeyHash)
import Data.PubKeyHash as PKH
import Data.String (fromCodePointArray)
import Data.Time.Duration (class Duration, Milliseconds(..), fromDuration)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UUID.Argonaut (UUID)
import Data.WalletId (WalletId)
import Data.WalletId as WI
import Data.WalletNickname (WalletNickname)
import Data.WalletNickname as WN
import Effect.Aff (Error, error)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Random (randomInt)
import Language.Marlowe.Client
  ( ContractHistory
  , EndpointResponse(..)
  , MarloweEndpointResult(..)
  , MarloweError
  )
import Marlowe.Run.Wallet.V1.CentralizedTestnet.Types
  ( CreatePostData(..)
  , RestorePostData(..)
  )
import Marlowe.Run.Wallet.V1.CentralizedTestnet.Types as Wallet
import Marlowe.Run.Wallet.V1.Types (WalletInfo(..))
import Marlowe.Semantics
  ( AccountId
  , Action(..)
  , Case(..)
  , ChoiceId
  , Contract(..)
  , CurrencySymbol
  , MarloweData(..)
  , MarloweParams(..)
  , Party(..)
  , Payee(..)
  , State(..)
  , Token(..)
  , TokenName
  , ValidatorHash
  , Value(..)
  , ValueId
  )
import Plutus.PAB.Webserver.Types (CombinedWSStreamToClient)
import Plutus.V1.Ledger.Time (POSIXTime(..))
import Plutus.V1.Ledger.Value as PV
import Safe.Coerce (coerce)
import Test.Data.Plutus (instanceUpdate, newObservableState)
import Unsafe.Coerce (unsafeCoerce)

-------------------------------------------------------------------------------
-- Webserver
-------------------------------------------------------------------------------

mnemonicPhrase :: forall m. MonadThrow Error m => String -> m MnemonicPhrase
mnemonicPhrase mp =
  expectRight ("Bad mnemonic phrase: " <> mp) $ MP.fromString mp

walletName :: forall m. MonadThrow Error m => String -> m WalletNickname
walletName name = expectRight ("Bad wallet name: " <> name) $ WN.fromString name

walletInfo :: WalletId -> Address -> PubKeyHash -> WalletInfo
walletInfo walletId address pubKeyHash = WalletInfo
  { walletId
  , address
  , pubKeyHash: PPKH.fromPubKeyHash pubKeyHash
  }

restoreRequest :: WalletNickname -> MnemonicPhrase -> RestorePostData
restoreRequest getRestoreWalletName getRestoreMnemonicPhrase = RestorePostData
  { getRestoreWalletName
  , getRestorePassphrase: fixmeAllowPassPerWallet
  , getRestoreMnemonicPhrase
  }

restoreResponse :: WalletId -> Address -> PubKeyHash -> WalletInfo
restoreResponse = walletInfo

createWalletRequest :: WalletNickname -> CreatePostData
createWalletRequest getCreateWalletName = CreatePostData
  { getCreateWalletName
  , getCreatePassphrase: fixmeAllowPassPerWallet
  }

createWalletResponse :: MnemonicPhrase -> WalletInfo -> Wallet.CreateResponse
createWalletResponse = map (map Wallet.CreateResponse)
  { mnemonic: _, walletInfo: _ }

-------------------------------------------------------------------------------
-- Endpoints
-------------------------------------------------------------------------------

createEndpoint = "create" :: String
applyInputsEndpoint = "apply-inputs-nonmerkleized" :: String
redeemEndpoint = "redeem" :: String
followEndpoint = "follow" :: String

marloweAppEndpoints :: Array String
marloweAppEndpoints = [ createEndpoint, redeemEndpoint, redeemEndpoint ]

followerEndpoints :: Array String
followerEndpoints = [ followEndpoint ]

companionEndpoints :: Array String
companionEndpoints = []

emptyEndpoints :: Array String
emptyEndpoints = []

createContent :: UUID -> Map TokenName Address -> Contract -> Array Json
createContent reqId roles contract =
  [ encodeJson reqId
  , encodeJson
      $ Map.fromFoldable
      $ map (lmap (PV.TokenName <<< { unTokenName: _ }))
      $ (Map.toUnfoldable roles :: Array _)
  , encodeJson contract
  ]

-------------------------------------------------------------------------------
-- MarloweApp Websocket Traffic
-------------------------------------------------------------------------------

type MarloweEndpointResponse =
  EndpointResponse MarloweEndpointResult MarloweError

-- Responses
createResponse :: MarloweParams -> MarloweEndpointResult
createResponse = CreateResponse

applyInputsResponse :: MarloweEndpointResult
applyInputsResponse = ApplyInputsResponse

redeemResponse :: MarloweEndpointResult
redeemResponse = RedeemResponse

endpointSuccess :: UUID -> MarloweEndpointResult -> MarloweEndpointResponse
endpointSuccess = EndpointSuccess

createSuccess :: UUID -> MarloweParams -> MarloweEndpointResponse
createSuccess reqId = endpointSuccess reqId <<< createResponse

applyInputsSuccess :: UUID -> MarloweEndpointResponse
applyInputsSuccess reqId = endpointSuccess reqId applyInputsResponse

redeemSuccess :: UUID -> MarloweEndpointResponse
redeemSuccess reqId = endpointSuccess reqId redeemResponse

endpointException :: String -> UUID -> MarloweError -> MarloweEndpointResponse
endpointException = flip EndpointException

createException :: UUID -> MarloweError -> MarloweEndpointResponse
createException = endpointException createEndpoint

applyInputsException :: UUID -> MarloweError -> MarloweEndpointResponse
applyInputsException = endpointException applyInputsEndpoint

redeemException :: UUID -> MarloweError -> MarloweEndpointResponse
redeemException = endpointException redeemEndpoint

-- WS message payloads
createSuccessMessage
  :: UUID -> UUID -> MarloweParams -> CombinedWSStreamToClient
createSuccessMessage appId reqId =
  instanceUpdate appId <<< newObservableState <<< createSuccess reqId

createExceptionMessage
  :: UUID -> UUID -> MarloweError -> CombinedWSStreamToClient
createExceptionMessage appId reqId =
  instanceUpdate appId <<< newObservableState <<< createException reqId

applyInputsSuccessMessage :: UUID -> UUID -> CombinedWSStreamToClient
applyInputsSuccessMessage appId =
  instanceUpdate appId <<< newObservableState <<< applyInputsSuccess

applyInputsExceptionMessage
  :: UUID -> UUID -> MarloweError -> CombinedWSStreamToClient
applyInputsExceptionMessage appId reqId =
  instanceUpdate appId <<< newObservableState <<< applyInputsException reqId

redeemSuccessMessage :: UUID -> UUID -> CombinedWSStreamToClient
redeemSuccessMessage appId =
  instanceUpdate appId <<< newObservableState <<< redeemSuccess

redeemExceptionMessage
  :: UUID -> UUID -> MarloweError -> CombinedWSStreamToClient
redeemExceptionMessage appId reqId =
  instanceUpdate appId <<< newObservableState <<< redeemException reqId

-------------------------------------------------------------------------------
-- WalletCompanion Websocket Traffic
-------------------------------------------------------------------------------

walletCompantionState
  :: forall f
   . Foldable f
  => f (Tuple MarloweParams MarloweData)
  -> Map MarloweParams MarloweData
walletCompantionState = Map.fromFoldable

walletCompantionMessage
  :: forall f
   . Foldable f
  => UUID
  -> f (Tuple MarloweParams MarloweData)
  -> CombinedWSStreamToClient
walletCompantionMessage appId =
  instanceUpdate appId <<< newObservableState <<< walletCompantionState

-------------------------------------------------------------------------------
-- MarloweFollower Websocket Traffic
-------------------------------------------------------------------------------

followerMessage :: UUID -> ContractHistory -> CombinedWSStreamToClient
followerMessage appId = instanceUpdate appId <<< newObservableState

-------------------------------------------------------------------------------
-- Semantic model
-------------------------------------------------------------------------------

marloweParams :: CurrencySymbol -> ValidatorHash -> MarloweParams
marloweParams unCurrencySymbol rolePayoutValidatorHash = MarloweParams
  { rolesCurrency: { unCurrencySymbol }
  , rolePayoutValidatorHash
  }

marloweData :: Contract -> State -> MarloweData
marloweData marloweContract marloweState = MarloweData
  { marloweContract
  , marloweState
  }

semanticState
  :: forall f g h
   . Foldable f
  => Functor f
  => Foldable g
  => Foldable h
  => f (Party /\ Token /\ Int)
  -> g (Tuple ChoiceId Int)
  -> h (Tuple ValueId Int)
  -> Instant
  -> State
semanticState accounts choices boundValues minTime = State
  { accounts: BigInt.fromInt <$> Map.fromFoldable (assocTuples <$> accounts)
  , choices: BigInt.fromInt <$> Map.fromFoldable choices
  , boundValues: BigInt.fromInt <$> Map.fromFoldable boundValues
  , minTime: POSIXTime minTime
  }

adaToken :: Token
adaToken = Token "" ""

mkWhen :: Instant -> Array Case -> Contract -> Contract
mkWhen timeout cases = When cases (POSIXTime timeout)

mkWhenClose :: Instant -> Array Case -> Contract
mkWhenClose timeout cases = mkWhen timeout cases Close

constant :: Int -> Value
constant = Constant <<< BigInt.fromInt

addConstant :: Int -> Int -> Value
addConstant a b = AddValue (constant a) (constant b)

depositAda :: AccountId -> Party -> Value -> Contract -> Case
depositAda account party = Case <<< Deposit account party adaToken

payPartyAda :: AccountId -> Party -> Value -> Contract -> Contract
payPartyAda account party value = Pay account (Party party) adaToken value

-------------------------------------------------------------------------------
-- Loan Contract
-------------------------------------------------------------------------------

borrowerTokenName :: TokenName
borrowerTokenName = "Borrower"

lenderTokenName :: TokenName
lenderTokenName = "Lender"

loanRoles :: Address -> Address -> Map TokenName Address
loanRoles borrowerAddress lenderAddress = Map.fromFoldable
  [ Tuple borrowerTokenName borrowerAddress
  , Tuple lenderTokenName lenderAddress
  ]

borrower :: Party
borrower = Role borrowerTokenName

lender :: Party
lender = Role lenderTokenName

loan :: Instant -> Instant -> Int -> Int -> Contract
loan loanDeadline repaymentDeadline interest amount =
  let
    loanValue = constant amount
    repaymentValue = interest `addConstant` amount
  in
    mkWhenClose loanDeadline
      [ depositAda lender lender loanValue do
          payPartyAda lender borrower loanValue do
            mkWhenClose repaymentDeadline do
              [ depositAda borrower borrower repaymentValue do
                  payPartyAda borrower lender repaymentValue Close
              ]
      ]

-------------------------------------------------------------------------------
-- Wallet Types
-------------------------------------------------------------------------------

walletNickname :: forall m. MonadThrow Error m => String -> m WalletNickname
walletNickname name =
  expectRight ("invalid wallet nickname: " <> name) $ WN.fromString name

newMnemonicPhrase
  :: forall m. MonadError Error m => MonadEffect m => m MnemonicPhrase
newMnemonicPhrase = do
  words <- replicateM 24 do
    word <- pickFrom English.dictionary
    expectJust ("invalid mnemonic phrase word: " <> word) $ Word.fromString word
  expectRight "Invalid mnemonic phrase"
    $ MP.fromWords
    $ Array.fromFoldable words

newWalletId :: forall m. MonadEffect m => MonadError Error m => m WalletId
newWalletId = do
  str <- fold <$> replicateM 40 newHexChar
  expectRight ("invalid mnemonic walletId was generated: " <> str)
    $ WI.fromString str

newPubKeyHash :: forall m. MonadEffect m => MonadError Error m => m PubKeyHash
newPubKeyHash = PKH.fromString <<< fold <$> replicateM 56 newHexChar

newAddress :: forall m. MonadEffect m => MonadError Error m => m Address
newAddress = do
  Bech32 <<< Bech32Address (unsafeCoerce "addr_test") <$> newDataPart
  where
  newDataPart = do
    codePoints <- Array.fromFoldable <$> replicateM 98 pickEnum
    let str = fromCodePointArray $ toCodePoint <$> codePoints
    expectJust
      ("invalid bech 32 data part was generated: " <> str)
      (BDP.fromCodePoints codePoints)

newWalletInfo :: forall m. MonadEffect m => MonadError Error m => m WalletInfo
newWalletInfo =
  WalletInfo <$>
    ( { walletId: _, pubKeyHash: _, address: _ }
        <$> newWalletId
        <*> (PPKH.fromPubKeyHash <$> newPubKeyHash)
        <*> newAddress
    )

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

newHexChar :: forall m. MonadEffect m => m String
newHexChar = do
  int <- liftRandomInt 0 15
  pure $ toStringAs hexadecimal int

liftRandomInt :: forall m. MonadEffect m => Int -> Int -> m Int
liftRandomInt i = liftEffect <<< randomInt i

pickFrom
  :: forall m a
   . MonadEffect m
  => MonadThrow Error m
  => Array a
  -> m a
pickFrom arr = do
  let len = length arr
  index <- liftRandomInt 0 len
  expectJust ("index out of bounds. " <> show { len, index }) $ arr !! index

pickEnum
  :: forall m a. MonadEffect m => MonadError Error m => BoundedEnum a => m a
pickEnum = do
  value <- liftRandomInt 0 $ coerce (cardinality :: _ a) - 1
  expectJust ("enum value out of bounds: " <> show value) $ toEnum value

expectRight :: forall m a b. MonadThrow Error m => String -> Either a b -> m b
expectRight msg = either (const $ fail $ "Test error: " <> msg) pure

expectJust :: forall m a. MonadThrow Error m => String -> Maybe a -> m a
expectJust msg = maybe (fail $ "Test error: " <> msg) pure

fail :: forall m a. MonadThrow Error m => String -> m a
fail = throwError <<< error

assocTuples :: forall a b c. a /\ (b /\ c) -> (a /\ b) /\ c
assocTuples (a /\ (b /\ c)) = ((a /\ b) /\ c)

fromNow
  :: forall d m
   . MonadThrow Error m
  => MonadTime m
  => Show d
  => Duration d
  => d
  -> m Instant
fromNow duration = adjustInstant duration =<< now

adjustInstant
  :: forall d m
   . MonadThrow Error m
  => Show d
  => Duration d
  => d
  -> Instant
  -> m Instant
adjustInstant duration i =
  expectJust ("failed to adjust " <> show i <> " by " <> show duration)
    $ instant
    $ over2 Milliseconds add (fromDuration duration)
    $ unInstant i
