module Natas.Natas where

import           Control.Lens
import           Data.ByteString.Lazy       (ByteString)
import           Data.Char                  (isDigit)
import           Data.String                (fromString)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as E
import qualified Data.Text.IO               as TIO
import           Network.Wreq

import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Syntax

type Answer = Maybe Text

type CResponse = Response ByteString

type Solution = IO Answer

parentUri :: Int -> String
parentUri level = "http://natas" ++ show level ++ ".natas.labs.overthewire.org"

accessLevel :: Int -> IO CResponse
accessLevel level = accessLevel' level (parentUri level) id

accessLevel' :: Int -> String -> (Options -> Options) -> IO CResponse
accessLevel' level uri modifyOptions = do
  opts <- modifyOptions <$> loginOptions level
  getWith opts uri

loginOptions :: Int -> IO Options
loginOptions level = do
  password <- E.encodeUtf8 <$> readPassword level
  let username = fromString ("natas" ++ show level)
  pure $ defaults & auth ?~ basicAuth username password

readPassword :: Int -> IO Text
readPassword n = T.strip <$> TIO.readFile ("passwords/natas" ++ show n)

writePassword :: Int -> Text -> IO ()
writePassword n = TIO.writeFile ("passwords/natas" ++ show n) . T.strip

moduleToSoln :: Module -> Q (Maybe ExpQ)
moduleToSoln (Module _ (ModName modname))
  | null moduleNumberStr = pure Nothing
  | otherwise = do
    solnName <- lookupValueName (modname ++ ".solution")
    case solnName of
      Nothing -> pure Nothing
      Just name ->
        pure $ pure (tupE [litE (integerL (read moduleNumberStr)), varE name])
  where
    moduleNumberStr = filter isDigit modname
