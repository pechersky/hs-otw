module Natas.Parse where

import           Data.Maybe        (catMaybes)
import           Data.Text         (Text)
import qualified Data.Text         as T
import           Safe              (headMay)
import           Text.HTML.TagSoup (Tag (TagComment), maybeTagText, parseTags)

fromTagComment :: Tag a -> Maybe a
fromTagComment =
  \case
    TagComment str -> Just str
    _tag -> Nothing

workupComments' :: ([Text] -> Bool) -> Text -> Maybe Text
workupComments' predicate body = last . T.words <$> targetTag
  where
    comments = (catMaybes . fmap fromTagComment . parseTags) body
    targetTag = (headMay . filter (predicate . T.words)) comments

workupComments :: Int -> Text -> Maybe Text
workupComments level = workupComments' (T.pack ("natas" ++ show level) `elem`)

workupBody' :: ([Text] -> Bool) -> Text -> [[Text]]
workupBody' predicate =
  filter predicate . fmap T.words . catMaybes . fmap maybeTagText . parseTags

workupBody :: Int -> Text -> Maybe [Text]
workupBody level = headMay . workupBody' (T.pack ("natas" ++ show level) `elem`)
