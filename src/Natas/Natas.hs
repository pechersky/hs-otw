module Natas.Natas where

import           Control.Lens
import           Data.ByteString.Lazy       (ByteString)
import           Data.Char                  (isDigit)
import           Data.String                (fromString)
import           Network.Wreq

import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Syntax

parentUri :: Int -> String
parentUri level = "http://natas" ++ show level ++ ".natas.labs.overthewire.org"

accessLevel :: Int -> String -> IO (Response ByteString)
accessLevel level (fromString -> password) = getWith opts (parentUri level)
  where
    opts = defaults & auth ?~ basicAuth username password
    username = fromString ("natas" ++ show level)

moduleToSoln :: Module -> Q (Maybe ExpQ)
moduleToSoln (Module _ (ModName modname))
  | null moduleNumberStr = pure Nothing
  | otherwise = do
    solnName <- lookupValueName (modname ++ ".solution")
    case solnName of
      Nothing -> pure Nothing
      Just name ->
        pure $ pure (tupE [litE (integerL (read moduleNumberStr)), varE name])
  where
    moduleNumberStr = filter isDigit modname
