-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Client for Isabelle-based Marlowe testing service.
--
-----------------------------------------------------------------------------


{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Spec.Marlowe.Service
  ( -- * Testing
    handle
  , handleValues
  ) where


import Spec.Marlowe.Service.Random (generateValue)
import Spec.Marlowe.Service.Serialization (roundtripSerialization)
import Spec.Marlowe.Service.Types (Request(..), Response(..))

import qualified Data.Aeson as A (Result(..), Value, fromJSON, object, toJSON, (.=))
import qualified Language.Marlowe.Core.V1.Semantics as Marlowe
  (computeTransaction, evalObservation, evalValue, playTrace)


-- | Respond to a request expressed as JSON.
handleValues :: A.Value -> IO A.Value
handleValues request =
  case A.fromJSON request of
    A.Success request' -> A.toJSON <$> handle request'
    A.Error message -> error message


-- | Respond to a request.
handle :: Request -> IO Response
handle TestRoundtripSerialization{..} =
  pure
    . RequestResponse . A.toJSON
    $ roundtripSerialization typeSerialized valueSerialized
handle GenerateRandomValue{..} =
  generateValue size seed typeSerialized
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
handle EvalValue{..} =
  let
    valueResponse = A.toJSON $ Marlowe.evalValue environment state value
  in
    pure RequestResponse{..}
handle EvalObservation{..} =
  let
    valueResponse = A.toJSON $ Marlowe.evalObservation environment state observation
  in
    pure RequestResponse{..}
