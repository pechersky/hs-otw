{-# LANGUAGE TemplateHaskell #-}

module Natas
  ( challenges
  , runChallenge
  ) where

import           Data.List                  (isInfixOf)
import qualified Data.Map                   as M
import           Data.Maybe                 (catMaybes)

import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Syntax

import           Natas.Natas
import           Natas.Natas0               (solution)

challenges :: M.Map Int Solution
challenges =
  M.fromList
    $(do currmod <- thisModule
         ModuleInfo xs <- reifyModule currmod
         let natasModules =
               filter
                 (\(Module _ mname) -> isInfixOf "Natas" (modString mname))
                 xs
         solns <- traverse moduleToSoln natasModules
         listE (catMaybes solns))

runChallenge :: Maybe Int -> IO ()
runChallenge =
  \case
    Nothing -> pure ()
    Just n ->
      case M.lookup n challenges of
        Nothing -> pure ()
        Just act -> do
          request <- accessLevel n
          act request >>= \case
            Nothing -> pure ()
            Just newPassword -> do
              print newPassword
              writePassword (n+1) newPassword
