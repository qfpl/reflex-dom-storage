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
import Data.Semigroup

import Control.Monad.Trans (lift)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State.Strict (StateT)
import qualified Control.Monad.State.Lazy as Lazy (StateT)

import Reflex

import Reflex.Dom.Routing.Nested
import Reflex.Dom.Routing.Writer

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Dependent.Map (DMap, Some(..), GCompare)
import qualified Data.Dependent.Map as DMap

data StorageMonoid k =
  StorageMonoid {
    smInserts :: DMap k Identity
  , smRemoves :: Set (Some k)
  }

instance GCompare k => Semigroup (StorageMonoid k) where
  (StorageMonoid i1 r1) <> (StorageMonoid i2 r2) =
    StorageMonoid (DMap.union i1 i2) (Set.union r1 r2)

instance GCompare k => Monoid (StorageMonoid k) where
  mempty = StorageMonoid DMap.empty Set.empty
  mappend = (<>)

smInsert :: GCompare k => k a -> a -> StorageMonoid k
smInsert k a = StorageMonoid (DMap.singleton k (Identity a)) mempty

smRemove :: k a -> StorageMonoid k
smRemove k = StorageMonoid DMap.empty (Set.singleton (This k))

storageMonoidToEndo :: GCompare k
                    => StorageMonoid k
                    -> DMap k Identity
                    -> DMap k Identity
storageMonoidToEndo (StorageMonoid inserts removes) =
  DMap.filterWithKey (\k _ -> Set.notMember (This k) removes) .
  DMap.union inserts

class Monad m => HasStorage t k m | m -> k, m -> t where
  askStorage  :: m (Dynamic t (DMap k Identity))
  tellStorage :: Event t (StorageMonoid k) -> m ()

instance HasStorage t k m => HasStorage t k (ReaderT r m) where
  askStorage = lift askStorage
  tellStorage = lift . tellStorage

instance HasStorage t k m => HasStorage t k (StateT r m) where
  askStorage = lift askStorage
  tellStorage = lift . tellStorage

instance HasStorage t k m => HasStorage t k (Lazy.StateT r m) where
  askStorage = lift askStorage
  tellStorage = lift . tellStorage

instance HasStorage t k m => HasStorage t k (RouteT t r m) where
  askStorage = lift askStorage
  tellStorage = lift . tellStorage

instance HasStorage t k m => HasStorage t k (RouteWriterT t r m) where
  askStorage = lift askStorage
  tellStorage = lift . tellStorage

tellStorageInsert :: (Reflex t, GCompare k, Monad m, HasStorage t k m)
                  => k a
                  -> Event t a
                  -> m ()
tellStorageInsert k e =
  tellStorage $ smInsert k <$> e

tellStorageRemove :: (Reflex t, Monad m, HasStorage t k m)
                  => k a
                  -> Event t ()
                  -> m ()
tellStorageRemove k e =
  tellStorage $ smRemove k <$ e

askStorageTag :: (Reflex t, GCompare k, Monad m, HasStorage t k m)
              => k a
              -> m (Dynamic t (Maybe a))
askStorageTag k = do
  dStorage <- askStorage
  pure $ fmap runIdentity . DMap.lookup k <$> dStorage

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
