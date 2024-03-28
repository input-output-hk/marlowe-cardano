{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Runtime.Web.Core.Object.Schema () where

import Language.Marlowe.Object.Types (Label (..))
import Language.Marlowe.Runtime.Web.Core.Semantics.Schema ()
import Servant (
  FromHttpApiData (parseUrlPiece),
  ToHttpApiData (toUrlPiece),
 )

instance ToHttpApiData Label where
  toUrlPiece = unLabel

instance FromHttpApiData Label where
  parseUrlPiece = pure . Label
