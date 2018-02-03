module Natas.Parse where

import           Text.HTML.TagSoup

fromTagComment :: Tag a -> Maybe a
fromTagComment =
  \case
    TagComment str -> Just str
    _ -> Nothing
