{-# LANGUAGE RankNTypes #-}

module Natas.Parse where

import           Data.Maybe        (catMaybes)

import           Data.Text         (Text)
import qualified Data.Text         as T
import           Safe              (headMay, lastMay)

import           Text.HTML.TagSoup (Tag (TagComment), maybeTagText, parseTags)

fromTagComment :: Tag a -> Maybe a
fromTagComment =
  \case
    TagComment str -> Just str
    _tag -> Nothing

filterMatch :: Int -> [[Text]] -> [[Text]]
filterMatch level = filter (T.pack ("natas" ++ show level) `elem`)

workupComments :: Int -> Text -> Maybe Text
workupComments level body = comments >>= lastMay
  where
    comments = headMay (workupTags fromTagComment (filterMatch level) body)

workupBody :: Int -> Text -> Maybe [Text]
workupBody level = headMay . workupBody' (filterMatch level)

workupBody' :: ([[Text]] -> a) -> Text -> a
workupBody' = workupTags maybeTagText

workupTags :: (forall str. Tag str -> Maybe str) -> ([[Text]] -> a) -> Text -> a
workupTags tagReader converter =
  converter . fmap T.words . catMaybes . fmap tagReader . parseTags
