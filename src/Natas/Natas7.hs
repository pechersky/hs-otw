{-# LANGUAGE OverloadedStrings #-}

module Natas.Natas7 where

import qualified Data.Text         as T
import           Safe              (lastMay)

import           Network.Wreq      (param)
import           Text.HTML.TagSoup (innerText, parseTags)

import           Control.Lens

import           Natas.Natas
import           Natas.Parse

solution :: Solution
solution = do
  sreq <- getLevel 7
  Just subpath <- pure $ workupComments 8 (reqBody sreq)
  req <- getLevel' 7 (parentUri 7) (param "page" .~ [subpath])
  let body = reqBody req
      match = (T.words . T.strip . innerText . parseTags) body
  pure $ lastMay match
