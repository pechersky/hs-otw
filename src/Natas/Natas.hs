module Natas.Natas where

import           Control.Lens
import           Data.ByteString.Lazy (ByteString)
import           Data.String          (fromString)
import           Network.Wreq

parentUri :: Int -> String
parentUri level = "http://natas" ++ show level ++ ".natas.labs.overthewire.org"

accessLevel :: Int -> String -> IO (Response ByteString)
accessLevel level (fromString -> password) = getWith opts (parentUri level)
  where
    opts = defaults & auth ?~ basicAuth username password
    username = fromString ("natas" ++ show level)
