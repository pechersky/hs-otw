module Natas.Natas1 where

import           Control.Lens
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Maybe                 (catMaybes, listToMaybe)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8)
import           Network.Wreq
import           Text.HTML.TagSoup

import           Natas.Natas
import           Natas.Parse

solution :: Solution
solution = do
  req <- accessLevel 1
  pure $ workupRequest req

workupRequest :: CResponse -> Answer
workupRequest r = last . T.words <$> targetTag
  where
    comments =
      fmap (decodeUtf8 . C.toStrict) .
      catMaybes . fmap fromTagComment . parseTags $
      r ^. responseBody
    targetTag = listToMaybe . filter (("natas2" `elem`) . T.words) $ comments
