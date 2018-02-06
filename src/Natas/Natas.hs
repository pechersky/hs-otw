module Natas.Natas where

import           Control.Lens
import           Data.ByteString.Lazy (ByteString, toStrict)
import           Data.String          (fromString)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Text.Encoding   (decodeUtf8, encodeUtf8)
import qualified Data.Text.IO         as TIO
import           Network.Wreq

type Solution = IO (Maybe Text)

parentUri :: Int -> String
parentUri level = "http://natas" ++ show level ++ ".natas.labs.overthewire.org"

accessLevel :: Int -> IO (Response ByteString)
accessLevel level = accessLevel' level (parentUri level) id

accessLevel' ::
     Int -> String -> (Options -> Options) -> IO (Response ByteString)
accessLevel' level uri modifyOptions = do
  opts <- modifyOptions <$> loginOptions level
  getWith opts uri

loginOptions :: Int -> IO Options
loginOptions level = do
  password <- encodeUtf8 <$> readPassword level
  let username = fromString ("natas" ++ show level)
  pure $ defaults & auth ?~ basicAuth username password

readPassword :: Int -> IO Text
readPassword n = T.strip <$> TIO.readFile ("passwords/natas" ++ show n)

writePassword :: Int -> Text -> IO ()
writePassword n = TIO.writeFile ("passwords/natas" ++ show n) . T.strip

reqBody :: Response ByteString -> Text
reqBody = decodeUtf8 . toStrict . view responseBody
