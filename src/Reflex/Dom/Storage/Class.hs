{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Reflex.Dom.Storage.Class where

import Data.Functor.Identity (Identity(..))
import Data.Maybe (isNothing, fromMaybe)

import Control.Monad.Trans (lift)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State.Strict (StateT)
import qualified Control.Monad.State.Lazy as Lazy (StateT)

import Reflex
import Data.Functor.Misc (ComposeMaybe(..))

import qualified Data.Dependent.Map as DMap
import Data.GADT.Compare (GCompare)

pdmInsert :: GCompare k => k a -> a -> PatchDMap k Identity
pdmInsert k =
  PatchDMap . DMap.singleton k . ComposeMaybe . Just . Identity

pdmRemove :: k a -> PatchDMap k Identity
pdmRemove k =
  PatchDMap . DMap.singleton k . ComposeMaybe $ Nothing

class Monad m => HasStorage t k m | m -> k, m -> t where
  askStorage  :: m (Incremental t (PatchDMap k Identity))
  tellStorage :: Event t (PatchDMap k Identity) -> m ()

instance HasStorage t k m => HasStorage t k (ReaderT r m) where
  askStorage = lift askStorage
  tellStorage = lift . tellStorage

instance HasStorage t k m => HasStorage t k (StateT r m) where
  askStorage = lift askStorage
  tellStorage = lift . tellStorage

instance HasStorage t k m => HasStorage t k (Lazy.StateT r m) where
  askStorage = lift askStorage
  tellStorage = lift . tellStorage

tellStorageInsert :: (Reflex t, GCompare k, Monad m, HasStorage t k m)
                  => k a
                  -> Event t a
                  -> m ()
tellStorageInsert k e =
  tellStorage $ pdmInsert k <$> e

tellStorageRemove :: (Reflex t, Monad m, HasStorage t k m)
                  => k a
                  -> Event t ()
                  -> m ()
tellStorageRemove k e =
  tellStorage $ pdmRemove k <$ e

askStorageTag :: (Reflex t, GCompare k, Monad m, HasStorage t k m)
              => k a
              -> m (Dynamic t (Maybe a))
askStorageTag k = do
  dStorage <- askStorage
  pure $ fmap runIdentity . DMap.lookup k <$> incrementalToDynamic dStorage

askStorageTagDef :: (Reflex t, GCompare k, Monad m, HasStorage t k m)
                 => k a
                 -> a
                 -> m (Dynamic t a)
askStorageTagDef k d =
  fmap (fromMaybe d) <$> askStorageTag k

initializeTag :: (GCompare k, Monad m, HasStorage t k m, PostBuild t m)
              => k a
              -> a
              -> m ()
initializeTag k v = do
  ePostBuild <- getPostBuild
  dTag <- askStorageTag k
  let
    eInsert = ffilter isNothing $ current dTag <@ ePostBuild

  tellStorageInsert k $ v <$ eInsert
  pure ()
