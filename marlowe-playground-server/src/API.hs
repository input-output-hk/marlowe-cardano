{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators      #-}

module API where

import qualified Auth
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.Haskell.Interpreter (InterpreterError, InterpreterResult)
import qualified Language.Marlowe.ACTUS.Domain.ContractTerms as A (ContractTerms)
import qualified Language.Marlowe.ACTUS.Domain.Schedule as A (CashFlow)
import Servant.API (Capture, Get, Header, Headers, JSON, NoContent, PlainText, Post, Raw, ReqBody, StdMethod (GET),
                    Verb, (:<|>), (:>))
import Web.Cookie (SetCookie)
import Webghc.Server (CompileRequest)

type API
     = "oracle" :> Capture "exchange" String :> Capture "pair" String :> Get '[JSON] Value
       :<|> "actus" :> ("generate" :> ReqBody '[ JSON] A.ContractTerms :> Post '[ JSON] String
                        :<|> "generate-static" :> ReqBody '[ JSON] A.ContractTerms :> Post '[ JSON] String
                        :<|> "cashflows" :> ReqBody '[ JSON] A.ContractTerms :> Post '[ JSON] [A.CashFlow])
       :<|> "compile" :> ReqBody '[JSON] CompileRequest :> Post '[JSON] (Either InterpreterError (InterpreterResult String))
       :<|> "logout" :> Get '[JSON] (Headers '[Header "Set-Cookie" SetCookie] Value)
