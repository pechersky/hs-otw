module Natas.Natas0 where

import           Control.Lens
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Maybe                 (catMaybes, listToMaybe)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8)
import           Network.Wreq
import           Text.HTML.TagSoup

import           Natas.Natas

soln0 :: IO (Maybe T.Text)
soln0 = workupRequest <$> accessLevel 0 "natas0"

workupRequest :: Response ByteString -> Maybe T.Text
workupRequest r = last . T.words <$> targetTag
  where
    comments =
      fmap (decodeUtf8 . C.toStrict) .
      catMaybes . fmap fromTagComment . parseTags $
      r ^. responseBody
    targetTag = listToMaybe . filter (("natas1" `elem`) . T.words) $ comments

fromTagComment :: Tag a -> Maybe a
fromTagComment =
  \case
    TagComment str -> Just str
    _ -> Nothing
