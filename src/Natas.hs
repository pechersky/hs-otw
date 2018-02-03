{-# LANGUAGE TemplateHaskell #-}

module Natas
  ( challenges
  , runChallenge
  ) where

import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Maybe  (MaybeT (..), runMaybeT)
import           Data.List                  (isInfixOf)
import qualified Data.Map                   as M
import           Data.Maybe                 (catMaybes)

import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Syntax hiding (lift)

import           Natas.Natas
import           Natas.Natas0               (solution)
import           Natas.Natas1               (solution)
import           Natas.Natas2               (solution)
import           Natas.Natas3               (solution)

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

runChallenge :: Int -> IO (Maybe ())
runChallenge n = runMaybeT $ do
  Just act <- pure $ M.lookup n challenges
  request <- lift $ accessLevel n
  newPassword <- MaybeT (act request)
  lift $ print newPassword
  lift $ writePassword (n + 1) newPassword
