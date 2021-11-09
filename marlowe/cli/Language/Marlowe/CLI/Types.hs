module Language.Marlowe.CLI.Types (
  Command(..)
) where


data Command =
    Export -- FIXME: This is a placeholder to access the original version of the code.
  | ExportValidator
    {
      magic         :: Maybe Int
    , stake         :: Maybe String
    , validatorFile :: FilePath
    , printAddress  :: Bool
    , printHash     :: Bool
    , printStats    :: Bool
    }
  | ExportDatum
    {
      accountHash     :: String
    , accountLovelace :: Integer
    , minimumSlot     :: Integer
    , datumFile       :: FilePath
    , printHash       :: Bool
    , printStats      :: Bool
    }
  | ExportRedeemer
    {
      minimumSlot  :: Integer
    , maximumSlot  :: Integer
    , redeemerFile :: FilePath
    , printHash    :: Bool
    , printStats   :: Bool
    }
    deriving (Eq, Ord, Read, Show)
