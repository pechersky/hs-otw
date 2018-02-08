{-# LANGUAGE OverloadedStrings #-}

module Natas.Natas6 where

import           Data.Char    (isAlpha, isSpace)

import qualified Data.Text    as T
import           Safe         (lastMay)

import           Network.Wreq (partBS, partText)

import           Natas.Natas
import           Natas.Parse

solution :: Solution
solution = do
  sreq <- getLevel' 6 (parentUri 6 ++ "/includes/secret.inc") id
  let sbody = reqBody sreq
      Just secret = (lastMay . T.words . T.filter validChar) sbody
      form = [partText "secret" secret, partBS "submit" "Submit"]
  req <- postLevel 6 form
  let body = reqBody req
      match = workupBody 7 body
  pure $ match >>= lastMay

validChar :: Char -> Bool
validChar x = isAlpha x || isSpace x
