module Test.Web.Config where

type Config =
  { testIdAttribute :: String
  , asyncUntilTimeout :: String
  , computedStyleSupportsPseudoElements :: Boolean
  , defaultHidden :: Boolean
  , showOriginalStackTrace :: Boolean
  }
