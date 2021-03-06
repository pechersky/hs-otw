{-# LANGUAGE OverloadedStrings #-}

module Natas.Natas8 where

import           Data.Char                (isAlphaNum)
import           Data.Maybe               (catMaybes)

import qualified Data.ByteString          as B
import           Data.ByteString.Base64   (decode)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Text.Encoding       (encodeUtf8)
import           Safe                     (headMay, lastMay)

import           Data.HexString           (hexString, toBytes)
import           Network.Wreq             (partBS)
import           Text.HTML.TagSoup        (innerText, maybeTagText,
                                           parseOptionsEntities, parseTags,
                                           parseTagsOptions)
import           Text.HTML.TagSoup.Entity (lookupEntity)

import           Natas.Natas
import           Natas.Parse

solution :: Solution
solution = do
  Just secret <- fmap (>>= decodeSecret) getSecret
  let form = [partBS "secret" secret, partBS "submit" "Submit"]
  req <- postLevel 8 form
  let body = reqBody req
      match = workupBody 9 body
  pure $ match >>= lastMay

getSecret :: IO (Maybe Text)
getSecret = do
  req <- getLevel' 8 (parentUri 8 ++ "/index-source.html") id
  let body = (T.unpack . T.replace "&nbsp;" " " . reqBody) req
      ipage =
        (parseTags .
         innerText . parseTagsOptions (parseOptionsEntities lookupEntity))
          body
      texts = (fmap words . catMaybes . fmap maybeTagText) ipage
      targetTag =
        (fmap (!! 2) . headMay . filter ("<?$encodedSecret" `elem`)) texts
      secret =
        fmap (takeWhile isAlphaNum . dropWhile (not . isAlphaNum)) targetTag
  pure $ T.pack <$> secret

decodeSecret :: Text -> Maybe B.ByteString
decodeSecret = toMaybe . decode . B.reverse . toBytes . hexString . encodeUtf8
  where
    toMaybe = either (const Nothing) pure
