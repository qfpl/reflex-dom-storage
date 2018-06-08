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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module Reflex.Dom.Storage.Class where

import Control.Monad (void, forM_)
import Data.Coerce (coerce)
import Data.Functor.Identity (Identity(..))
import Data.Maybe (isNothing, catMaybes, fromMaybe)
import Data.Monoid hiding ((<>))
import Data.Proxy (Proxy(..))
import Data.Semigroup

import Control.Monad.Trans (MonadTrans, MonadIO, lift, liftIO)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Ref (MonadRef(..), MonadAtomicRef)
import Control.Monad.Exception (MonadAsyncException, MonadException)
import Control.Monad.Reader (ReaderT, runReaderT, ask)

import Reflex
import Reflex.Network
import Reflex.Host.Class
import Reflex.Dom.Core hiding (Value, Error, Window)

import Data.Text (Text)
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Dependent.Map (DMap, Some(..), GCompare)
import qualified Data.Dependent.Map as DMap

import Data.Aeson (ToJSON, FromJSON, Value, encode)
import Data.Aeson.Types (Parser, Result(..), parse)
import Data.Aeson.Encoding as E (value, encodingToLazyByteString)
import qualified Data.ByteString.Lazy as LBS

import GHCJS.DOM (currentWindowUnchecked)
import GHCJS.DOM.Types (MonadJSM, liftJSM, toJSVal)
import GHCJS.DOM.EventM (EventM, on)
import GHCJS.DOM.Window (Window, getLocalStorage, getSessionStorage)
import GHCJS.DOM.WindowEventHandlers (storage)
import GHCJS.DOM.Storage (Storage(..), getItem, setItem, removeItem)
import GHCJS.DOM.StorageEvent

import Language.Javascript.JSaddle (valToJSON)

import Reflex.Dom.Builder.Immediate (wrapDomEvent)
import Foreign.JavaScript.Utils (jsonDecode)

import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.ByteString.Lazy (toStrict, fromStrict)

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
storageMonoidToEndo (StorageMonoid inserts removes) d =
  let
    toRemove k = Set.member (This k) removes
  in
    DMap.filterWithKey (\k _ -> Set.notMember (This k) removes) .
    DMap.union inserts $
    d

class HasStorage t k m | m -> k, m -> t where
  askStorage  :: m (Dynamic t (DMap k Identity))
  tellStorage :: Event t (StorageMonoid k) -> m ()

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
  pure $ fmap runIdentity . (DMap.lookup k) <$> dStorage

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
