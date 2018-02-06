{-# LANGUAGE OverloadedStrings #-}

module Natas.Natas8 where

import           Data.Char                (isAlphaNum)
import           Data.Maybe               (catMaybes)
import           Data.String              (fromString)

import qualified Data.ByteString          as B
import           Data.ByteString.Base64   (decode)
import           Data.ByteString.Lazy     (ByteString)
import qualified Data.Text                as T
import           Safe                     (headMay, lastMay)

import           Data.HexString           (hexString, toBytes)
import           Network.Wreq             (FormParam ((:=)), postWith)
import           Text.HTML.TagSoup        (innerText, maybeTagText,
                                           parseOptionsEntities, parseTags,
                                           parseTagsOptions)
import           Text.HTML.TagSoup.Entity (lookupEntity)

import           Natas.Natas
import           Natas.Parse

solution :: Solution
solution = do
  Just secret <- fmap (>>= decodeSecret) getSecret
  let form = ["secret" := secret, "submit" := ("Submit" :: ByteString)]
  opts <- loginOptions 8
  req <- postWith opts (parentUri 8) form
  let body = reqBody req
      match = workupBody 9 body
  pure $ match >>= lastMay

getSecret :: IO (Maybe String)
getSecret = do
  req <- accessLevel' 8 (parentUri 8 ++ "/index-source.html") id
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
  pure secret

decodeSecret :: String -> Maybe B.ByteString
decodeSecret = toMaybe . decode . B.reverse . toBytes . hexString . fromString
  where
    toMaybe = either (const Nothing) pure
