module Transcript where

import Prologue

import Data.Argonaut (JsonDecodeError, encodeJson, stringifyWithIndent)
import Data.Maybe (fromMaybe)
import Data.String
  ( Pattern(..)
  , Replacement(..)
  , joinWith
  , replaceAll
  , stripPrefix
  , stripSuffix
  )
import Data.Time.Duration (Milliseconds)
import Plutus.PAB.Webserver.Types
  ( CombinedWSStreamToClient
  , CombinedWSStreamToServer
  )

data TranscriptEvent
  = WebSocketMsgReceived (Either JsonDecodeError CombinedWSStreamToClient)
  | WebSocketMsgSent CombinedWSStreamToServer

instance Show TranscriptEvent where
  show event =
    joinWith ""
      [ "("
      , case event of
          WebSocketMsgReceived (Left error) ->
            joinWith " $ " [ "WebSocketMsgReceived", "Left", show error ]
          WebSocketMsgReceived (Right msg) ->
            joinWith " $ "
              [ "WebSocketMsgReceived"
              , "parseDecodeJson"
              , joinWith ""
                  [ "\"\"\""
                  , replaceAll (Pattern "\n") (Replacement "\n          ")
                      $ stringifyWithIndent 2
                      $ encodeJson msg
                  , "\"\"\""
                  ]
              ]
          WebSocketMsgSent msg ->
            joinWith " $ "
              [ "WebSocketMsgSent"
              , "unsafeParseJson"
              , joinWith ""
                  [ "\"\"\""
                  , replaceAll (Pattern "\n") (Replacement "\n          ")
                      $ stringifyWithIndent 2
                      $ encodeJson msg
                  , "\"\"\""
                  ]
              ]
      , ")"
      ]

newtype Transcript = Transcript (Array (Tuple Milliseconds TranscriptEvent))

instance Show Transcript where
  show (Transcript events) = joinWith "\n "
    [ "Transcript"
    , joinWith ""
        [ "[ ", joinWith "\n  , " $ stripParens <<< show <$> events, "\n]" ]
    ]
    where
    stripParens s =
      let
        noRParen = fromMaybe s $ stripSuffix (Pattern ")") s
      in
        fromMaybe noRParen $ stripPrefix (Pattern "(") noRParen
