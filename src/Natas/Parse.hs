module Natas.Parse where

import           Control.Lens
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Maybe                 (catMaybes, listToMaybe)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8)
import           Network.Wreq
import           Text.HTML.TagSoup

import           Natas.Natas

fromTagComment :: Tag a -> Maybe a
fromTagComment =
  \case
    TagComment str -> Just str
    _ -> Nothing

workupComments :: ([T.Text] -> Bool) -> CResponse -> Answer
workupComments predicate req = last . T.words <$> targetTag
  where
    comments =
      fmap (decodeUtf8 . C.toStrict) .
      catMaybes . fmap fromTagComment . parseTags $
      req ^. responseBody
    targetTag = listToMaybe . filter (predicate . T.words) $ comments
