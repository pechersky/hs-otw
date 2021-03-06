{-# LANGUAGE OverloadedStrings #-}

module Natas.Natas3 where

import qualified Data.Text   as T
import           Safe        (headMay, lastMay)

import           Natas.Natas

solution :: Solution
solution = do
  req <- getLevel' 3 (parentUri 3 ++ "/s3cr3t/users.txt") id
  let body = reqBody req
      match =
        (headMay . filter ("natas4" `elem`) . fmap (T.splitOn ":") . T.lines)
          body
  pure $ match >>= lastMay
