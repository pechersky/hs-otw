module Natas.Natas8 where

import           Control.Lens
import qualified Data.ByteString            as B
import           Data.ByteString.Base64     (decode)
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Char                  (isAlphaNum)
import           Data.HexString
import           Data.Maybe                 (catMaybes, listToMaybe)
import           Data.String                (fromString)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8)
import           Network.Wreq
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Entity   (lookupEntity)

import           Natas.Natas

solution :: Solution
solution = do
  Just secret <- fmap (>>= decodeSecret) getSecret
  opts <- loginOptions 8
  req <-
    postWith
      opts
      (parentUri 8)
      ["secret" := secret, "submit" := ("Submit" :: C.ByteString)]
  let body = (decodeUtf8 . C.toStrict) $ req ^. responseBody
      match =
        (filter ("natas9" `elem`) .
         fmap T.words . catMaybes . fmap maybeTagText . parseTags)
          body
  pure $ listToMaybe match >>= (listToMaybe . reverse)

getSecret :: IO (Maybe String)
getSecret = do
  req <- accessLevel' 8 (parentUri 8 ++ "/index-source.html") id
  let body =
        (T.unpack . T.replace "&nbsp;" " " . decodeUtf8 . C.toStrict) $
        req ^. responseBody
      ipage =
        (parseTags .
         innerText . parseTagsOptions (parseOptionsEntities lookupEntity))
          body
      texts = (fmap words . catMaybes . fmap maybeTagText) ipage
      targetTag = (listToMaybe . filter ("<?$encodedSecret" `elem`)) texts
      secret =
        fmap
          (takeWhile isAlphaNum . dropWhile (not . isAlphaNum) . (!! 2))
          targetTag
  pure secret

decodeSecret :: String -> Maybe B.ByteString
decodeSecret = toMaybe . decode . B.reverse . toBytes . hexString . fromString
  where
    toMaybe = either (const Nothing) pure
