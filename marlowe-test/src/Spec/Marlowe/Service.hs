

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Spec.Marlowe.Service
  ( handle
  , handleValues
  ) where


import Spec.Marlowe.Service.Random (generateValue)
import Spec.Marlowe.Service.Serialization (roundtripSerialization)
import Spec.Marlowe.Service.Types (Request(..), Response(..))

import qualified Data.Aeson as A (Result(..), Value, fromJSON, object, toJSON, (.=))
import qualified Language.Marlowe.Core.V1.Semantics as Marlowe (computeTransaction, playTrace)


handleValues :: A.Value -> IO A.Value
handleValues request =
  case A.fromJSON request of
    A.Success request' -> A.toJSON <$> handle request'
    A.Error message -> error message


handle :: Request -> IO Response
handle TestRoundtripSerialization{..} =
  pure
    . RequestResponse . A.toJSON
    $ roundtripSerialization typeSerialized valueSerialized
handle GenerateRandomValue{..} =
  generateValue typeSerialized
    >>= \case
      Right value -> pure . RequestResponse . A.object . pure $ "value" A..= value
      Left failureResponse -> pure $ ResponseFailure{..}
handle ComputeTransaction{..} =
  let
    valueResponse = A.toJSON $ Marlowe.computeTransaction transactionInput state contract
  in
    pure RequestResponse{..}
handle PlayTrace{..} =
  let
    valueResponse = A.toJSON $ Marlowe.playTrace initialTime contract transactionInputs
  in
    pure RequestResponse{..}
