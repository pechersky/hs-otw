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
  sreq <- accessLevel 7
  let Just subpath = workupComments 8 (reqBody sreq)
      page = param "page" .~ [subpath]
  req <- accessLevel' 7 (parentUri 7) page
  let body = reqBody req
      match = (T.words . T.strip . innerText . parseTags) body
  pure $ lastMay match
