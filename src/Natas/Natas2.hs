module Natas.Natas2 where

import qualified Data.Text   as T
import           Safe        (headMay, lastMay)

import           Natas.Natas
import           Natas.Parse

solution :: Solution
solution = do
  req <- accessLevel' 2 (parentUri 2 ++ "/files/users.txt") id
  let body = reqBody req
      match =
        (headMay . filter ("natas3" `elem`) . fmap (T.splitOn ":") . T.lines)
          body
  pure $ match >>= lastMay
