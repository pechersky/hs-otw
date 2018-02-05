module Natas.Parse where

import           Control.Lens
import           Data.ByteString.Lazy.Char8 (toStrict)
import           Data.Maybe                 (catMaybes)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8)
import           Network.Wreq               (responseBody)
import           Safe                       (headMay)
import           Text.HTML.TagSoup          (Tag (TagComment), maybeTagText,
                                             parseTags)

import           Natas.Natas

reqBody :: CResponse -> T.Text
reqBody = decodeUtf8 . toStrict . view responseBody

fromTagComment :: Tag a -> Maybe a
fromTagComment =
  \case
    TagComment str -> Just str
    _tag -> Nothing

workupComments' :: ([T.Text] -> Bool) -> CResponse -> Answer
workupComments' predicate req = last . T.words <$> targetTag
  where
    comments = (catMaybes . fmap fromTagComment . parseTags . reqBody) req
    targetTag = (headMay . filter (predicate . T.words)) comments

workupComments :: Int -> CResponse -> Answer
workupComments level = workupComments' (T.pack ("natas" ++ show level) `elem`)

workupBody' :: ([T.Text] -> Bool) -> T.Text -> [[T.Text]]
workupBody' predicate =
  filter predicate . fmap T.words . catMaybes . fmap maybeTagText . parseTags

workupBody :: Int -> T.Text -> Maybe [T.Text]
workupBody level = headMay . workupBody' (T.pack ("natas" ++ show level) `elem`)
