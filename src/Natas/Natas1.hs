module Natas.Natas1 where

import           Control.Lens
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Maybe                 (catMaybes, listToMaybe)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8)
import           Network.Wreq
import           Text.HTML.TagSoup

import           Natas.Natas
import           Natas.Parse

solution :: Solution
solution = pure . workupRequest

workupRequest :: Response ByteString -> Answer
workupRequest r = last . T.words <$> targetTag
  where
    comments =
      fmap (decodeUtf8 . C.toStrict) .
      catMaybes . fmap fromTagComment . parseTags $
      r ^. responseBody
    targetTag = listToMaybe . filter (("natas2" `elem`) . T.words) $ comments
