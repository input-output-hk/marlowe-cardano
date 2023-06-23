{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Marlowe.Runtime.CLI.Command.Load (
  LoadCommand (..),
  loadCommandParser,
  runLoadCommand,
) where

import Control.Monad (foldM, unless)
import qualified Control.Monad as Monad
import Control.Monad.Except (ExceptT (ExceptT), runExceptT, throwError, withExceptT)
import Control.Monad.Reader (MonadReader (ask), ReaderT, local, runReaderT)
import Control.Monad.Trans (lift)
import Data.Aeson hiding (Value)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types hiding (Value)
import Data.List (sortOn)
import qualified Data.Text as T
import Data.Yaml (ParseException, decodeFileEither, prettyPrintParseException)
import Language.Marlowe.Core.V1.Semantics.Types (AccountId, Action, Observation, Payee, Timeout, Token, Value, ValueId)
import Language.Marlowe.Protocol.Load.Client (
  ClientStCanPush (..),
  ClientStComplete (..),
  ClientStPop,
  ClientStProcessing (..),
  MarloweLoadClient (MarloweLoadClient),
 )
import Language.Marlowe.Protocol.Load.Types (Node (..))
import Language.Marlowe.Runtime.CLI.Monad (CLI)
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash)
import Language.Marlowe.Runtime.Client (runMarloweLoadClient)
import Network.TypedProtocol (N (..), Nat (..))
import Options.Applicative (ParserInfo, help, info, metavar, progDesc, strArgument)
import Plutus.V2.Ledger.Api (POSIXTime (..))
import System.Exit (exitFailure)
import System.FilePath (takeDirectory)
import System.IO (hFlush, hPutStrLn, stderr)
import Text.Printf (hPrintf)
import UnliftIO (MonadUnliftIO, atomicModifyIORef, liftIO, newIORef)
import UnliftIO.Directory (doesFileExist, makeAbsolute, withCurrentDirectory)

newtype LoadCommand = LoadCommand
  { contractFile :: FilePath
  }

loadCommandParser :: ParserInfo LoadCommand
loadCommandParser = info parser $ progDesc "Load a contract into the runtime"
  where
    parser =
      LoadCommand
        <$> contractFileOption
    contractFileOption =
      strArgument $
        mconcat
          [ metavar "FILE_PATH"
          , help "A file that contains the JSON representation of the contract to load."
          ]

runLoadCommand :: LoadCommand -> CLI ()
runLoadCommand LoadCommand{..} = do
  result <- runExceptT $ flip runReaderT [] do
    nodeCount <- countNodes 0 contractFile
    progress <- newIORef (-1 :: Int)
    let countWidth = floor (logBase 10 $ realToFrac nodeCount :: Double) + 1 :: Int
        printStr = " [%-32s] %" <> show countWidth <> "d of " <> show nodeCount <> " nodes transferred.\r"
        incrementProgress = do
          newProgress <- atomicModifyIORef progress \i -> (i + 1, i + 1)
          let bar
                | newProgress == 0 = ""
                | newProgress == nodeCount = replicate 32 '='
                | otherwise = reverse $ '>' : replicate ((newProgress * 32 `div` nodeCount) - 1) '='
          liftIO $ hPrintf stderr printStr bar newProgress
          liftIO $ hFlush stderr
    lift $ ExceptT $ runMarloweLoadClient $ loadClient incrementProgress contractFile
  liftIO case result of
    Left err -> do
      case err of
        FileNotFound [] path -> do
          hPrintf stderr "Cannot find contract file %s" path
        FileNotFound (referencingFile : _) path -> do
          hPrintf stderr "Cannot find contract file %s imported by %s" path referencingFile
        FileInvalid referencingFile decodeError -> do
          hPrintf stderr "Error in file %s:\n%s" referencingFile $ prettyPrintParseException decodeError
        CyclicImport path -> do
          hPrintf stderr "Cyclic import detected of file %s" path
      hPutStrLn stderr ""
      exitFailure
    Right hash -> do
      hPutStrLn stderr "\nDone."
      putStrLn $ read $ show hash

type CountM = ReaderT [FilePath] (ExceptT LoadError CLI)

countNodes :: Int -> FilePath -> CountM Int
countNodes count path = withContract path $ countNodes' count

countNodes' :: Int -> LoadContract -> CountM Int
countNodes' count = \case
  Close -> pure $ count + 1
  Pay _ _ _ _ c -> countNodes' (count + 1) c
  If _ l r -> do
    countL <- countNodes' (count + 1) l
    countNodes' countL r
  When cases _ c -> do
    count' <- foldM countCase (count + 1) cases
    countNodes' count' c
  Let _ _ c -> countNodes' (count + 1) c
  Assert _ c -> countNodes' (count + 1) c
  Import path -> countNodes count path

countCase :: Int -> LoadCase -> CountM Int
countCase count (LoadCase _ c) = countNodes' (count + 1) c

withContract :: FilePath -> (LoadContract -> CountM a) -> CountM a
withContract path m = do
  breadcrumb <- ask
  (breadcrumb', contract) <- lift $ openFile breadcrumb path
  local (const breadcrumb') $ m contract

openFile :: (MonadUnliftIO m) => [FilePath] -> FilePath -> ExceptT LoadError m ([FilePath], LoadContract)
openFile breadcrumb path = do
  fileExists <- doesFileExist path
  unless fileExists $ throwError $ FileNotFound breadcrumb path
  absPath <- case breadcrumb of
    [] -> makeAbsolute path
    current : _ -> lift $ withCurrentDirectory (takeDirectory current) $ makeAbsolute path
  Monad.when (absPath `elem` breadcrumb) $ throwError $ CyclicImport absPath
  contract <-
    withExceptT (FileInvalid absPath) $
      ExceptT $
        liftIO $
          decodeFileEither absPath
  pure (absPath : breadcrumb, contract)

data LoadError
  = FileNotFound [FilePath] FilePath
  | FileInvalid FilePath ParseException
  | CyclicImport FilePath

loadClient
  :: CLI ()
  -> FilePath
  -> MarloweLoadClient CLI (Either LoadError DatumHash)
loadClient incrementProgress rootFile =
  MarloweLoadClient do
    incrementProgress
    pure $ processing [] (Import rootFile) StateRoot
  where
    processing
      :: [FilePath]
      -> LoadContract
      -> ClientState node
      -> ClientStProcessing node CLI (Either LoadError DatumHash)
    processing breadcrumb contract state = ClientStProcessing \n ->
      push breadcrumb contract state n

    push
      :: [FilePath]
      -> LoadContract
      -> ClientState node
      -> Nat n
      -> CLI (ClientStCanPush n node CLI (Either LoadError DatumHash))
    push breadcrumb contract state Zero = do
      pure $
        RequestResume $
          processing breadcrumb contract state
    push breadcrumb contract state (Succ n) = case contract of
      Close -> do
        incrementProgress
        pure $ PushClose $ pop breadcrumb state n
      Pay payee payor token value next -> do
        incrementProgress
        pure $ PushPay payee payor token value $ push breadcrumb next (StatePay state) n
      If obs tru fal -> do
        incrementProgress
        pure $ PushIf obs $ push breadcrumb tru (StateIfL fal state) n
      When cases timeout fallback -> do
        incrementProgress
        pure $ PushWhen timeout case cases of
          [] -> push breadcrumb fallback (StateWhen state) n
          (c : cs) -> pushCase breadcrumb n c cs fallback state
      Let valueId value next -> do
        incrementProgress
        pure $ PushLet valueId value $ push breadcrumb next (StateLet state) n
      Assert obs next -> do
        incrementProgress
        pure $ PushAssert obs $ push breadcrumb next (StateAssert state) n
      Import path -> do
        result <- runExceptT $ openFile breadcrumb path
        case result of
          Left err -> pure $ Abort $ Left err
          Right (breadcrumb', contract') -> push breadcrumb' contract' (StateImport breadcrumb state) (Succ n)

    pop
      :: [FilePath]
      -> ClientState node
      -> Nat n
      -> CLI (ClientStPop n node CLI (Either LoadError DatumHash))
    pop breadcrumb state n = case state of
      StateRoot -> pure $ ClientStComplete $ pure . Right
      StatePay state' -> pop breadcrumb state' n
      StateIfL fal state' -> push breadcrumb fal (StateIfR state') n
      StateIfR state' -> pop breadcrumb state' n
      StateWhen state' -> pop breadcrumb state' n
      StateCase [] fallback state' -> push breadcrumb fallback (StateWhen state') n
      StateCase (c : cs) fallback state' -> pushCase breadcrumb n c cs fallback state'
      StateLet state' -> pop breadcrumb state' n
      StateAssert state' -> pop breadcrumb state' n
      StateImport prevBreadcrumb state' -> pop prevBreadcrumb state' n

    pushCase
      :: [FilePath]
      -> Nat n
      -> LoadCase
      -> [LoadCase]
      -> LoadContract
      -> ClientState node
      -> CLI (ClientStCanPush n ('WhenNode node) CLI (Either LoadError DatumHash))
    pushCase breadcrumb n c cs fallback state = case n of
      Zero -> pure $ RequestResume $ ClientStProcessing \n' ->
        pushCase' breadcrumb n' c cs fallback state
      Succ n' -> pushCase' breadcrumb (Succ n') c cs fallback state

    pushCase'
      :: [FilePath]
      -> Nat ('S n)
      -> LoadCase
      -> [LoadCase]
      -> LoadContract
      -> ClientState node
      -> CLI (ClientStCanPush ('S n) ('WhenNode node) CLI (Either LoadError DatumHash))
    pushCase' breadcrumb (Succ n) c cs fallback state = case c of
      LoadCase action next -> do
        incrementProgress
        pure $ PushCase action $ push breadcrumb next (StateCase cs fallback state) n

data ClientState (node :: Node) where
  StateRoot :: ClientState 'RootNode
  StatePay :: ClientState node -> ClientState ('PayNode node)
  StateIfL :: LoadContract -> ClientState node -> ClientState ('IfLNode node)
  StateIfR :: ClientState node -> ClientState ('IfRNode node)
  StateWhen :: ClientState node -> ClientState ('WhenNode node)
  StateCase :: [LoadCase] -> LoadContract -> ClientState node -> ClientState ('CaseNode node)
  StateLet :: ClientState node -> ClientState ('LetNode node)
  StateAssert :: ClientState node -> ClientState ('AssertNode node)
  StateImport :: [FilePath] -> ClientState node -> ClientState node

-- A contract that can be loaded. It lacks merkleization and supports
-- file-based imports.
data LoadContract
  = Close
  | Pay AccountId Payee Token (Value Observation) LoadContract
  | If Observation LoadContract LoadContract
  | When [LoadCase] Timeout LoadContract
  | Let ValueId (Value Observation) LoadContract
  | Assert Observation LoadContract
  | Import FilePath

data LoadCase = LoadCase Action LoadContract

instance FromJSON LoadCase where
  parseJSON = withObject "Case" \obj ->
    LoadCase <$> (obj .: "case") <*> (obj .: "then")

instance FromJSON LoadContract where
  parseJSON = \case
    String "close" -> pure Close
    Object
      ( KM.toList ->
          [ ("import", String path)
            ]
        ) -> pure $ Import $ T.unpack path
    Object
      ( sortOn fst . KM.toList ->
          [ ("from_account", account)
            , ("pay", pay)
            , ("then", then_)
            , ("to", to)
            , ("token", token)
            ]
        ) ->
        Pay
          <$> parseJSON account <?> Key "from_account"
          <*> parseJSON to <?> Key "to"
          <*> parseJSON token <?> Key "token"
          <*> parseJSON pay <?> Key "pay"
          <*> parseJSON then_ <?> Key "then"
    Object
      ( sortOn fst . KM.toList ->
          [ ("else", else_)
            , ("if", if_)
            , ("then", then_)
            ]
        ) ->
        If
          <$> parseJSON if_ <?> Key "if"
          <*> parseJSON then_ <?> Key "then"
          <*> parseJSON else_ <?> Key "else"
    Object
      ( sortOn fst . KM.toList ->
          [ ("timeout", timeout)
            , ("timeout_continuation", continuation)
            , ("when", when)
            ]
        ) ->
        When
          <$> parseJSON when <?> Key "when"
          <*> (POSIXTime <$> parseJSON timeout <?> Key "timeout")
          <*> parseJSON continuation <?> Key "timeout_continuation"
    Object
      ( sortOn fst . KM.toList ->
          [ ("be", value)
            , ("let", valueId)
            , ("then", then_)
            ]
        ) ->
        Let
          <$> parseJSON valueId <?> Key "let"
          <*> parseJSON value <?> Key "be"
          <*> parseJSON then_ <?> Key "then"
    Object
      ( sortOn fst . KM.toList ->
          [ ("assert", obs)
            , ("then", then_)
            ]
        ) ->
        Assert
          <$> parseJSON obs <?> Key "assert"
          <*> parseJSON then_ <?> Key "then"
    _ -> fail "Expected an object or \"close\""
