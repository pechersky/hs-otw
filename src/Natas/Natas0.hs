module Natas.Natas0 where

import           Control.Lens
import           Network.Wreq
import           Text.HTML.TagSoup

import Natas.Natas

soln0 = do
  r <- accessLevel 0 "natas0"
  pure $ parseTags (r ^. responseBody)

fromTagComment = \case
  TagComment str -> Just str
  _              -> Nothing
