module Natas.Natas1 where

import           Natas.Natas
import           Natas.Parse

solution :: Solution
solution = do
  req <- getLevel 1
  pure $ workupComments 2 (reqBody req)
