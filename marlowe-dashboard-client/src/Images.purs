module Images
  ( arrowBack
  , backgroundShape
  , cfdIcon
  , contractIcon
  , getStartedThumbnail
  , linkHighlight
  , loanIcon
  , marloweRunLogo
  , marloweRunNavLogo
  , marloweRunNavLogoDark
  , purchaseIcon
  ) where

import Halogen.HTML (HTML, img)
import Halogen.HTML.Properties (src)
import Marlowe.Extended (ContractType(..))

foreign import marloweRunLogo :: String

foreign import marloweRunNavLogo :: String

foreign import marloweRunNavLogoDark :: String

foreign import backgroundShape :: String

foreign import arrowBack :: String

foreign import linkHighlight :: String

foreign import getStartedThumbnail :: String

foreign import cfdIcon :: String

foreign import loanIcon :: String

foreign import purchaseIcon :: String

contractIcon :: forall p a. ContractType -> HTML p a
contractIcon contractType =
  img
    [ src case contractType of
        Escrow -> purchaseIcon
        ZeroCouponBond -> loanIcon
        _ -> cfdIcon
    ]

