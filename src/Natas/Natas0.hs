module Natas.Natas0 where

import           Natas.Natas
import           Natas.Parse

solution :: Solution
solution = do
  req <- getLevel 0
  pure $ workupComments 1 (reqBody req)
