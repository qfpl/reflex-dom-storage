{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module Reflex.Dom.Storage.Base where

import Control.Monad (void)
import Data.Coerce (coerce)
import Data.Foldable (traverse_)
import Data.Functor.Identity (Identity(..))
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy(..))
import Data.Semigroup ((<>))

import Control.Monad.Trans (MonadTrans, MonadIO, lift)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Ref (MonadRef(..), MonadAtomicRef)
import Control.Monad.Reader (ReaderT, runReaderT, ask)

import Reflex
import Reflex.Host.Class
import Reflex.Dom.Core hiding (Value, Error, Window)

import Data.Text (Text)
import qualified Data.Set as Set

import Data.Dependent.Map (DMap, Some(..), GCompare)
import qualified Data.Dependent.Map as DMap

import qualified Data.ByteString.Lazy as LBS

import GHCJS.DOM (currentWindowUnchecked)
import GHCJS.DOM.Types (MonadJSM)
import GHCJS.DOM.EventM (EventM, on)
import GHCJS.DOM.Window (Window, getLocalStorage, getSessionStorage)
import GHCJS.DOM.WindowEventHandlers (storage)
import GHCJS.DOM.Storage (Storage(..), getItem, setItem, removeItem)
import GHCJS.DOM.StorageEvent

import Reflex.Dom.Builder.Immediate (wrapDomEvent)

import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.ByteString.Lazy (toStrict, fromStrict)

import Reflex.Dom.Storage.Class

class GKey t where
  toKey :: Some t -> Text
  fromKey :: Text -> Maybe (Some t)
  keys :: Proxy t -> [Some t]

class ToJSONTag t f where
  encodeTagged :: t a -> f a -> LBS.ByteString

class FromJSONTag t f where
  decodeTagged :: t a -> LBS.ByteString -> Maybe (f a)

data StorageType =
    SessionStorage
  | LocalStorage
  deriving (Eq, Ord, Show)

newtype StorageT t k m a =
  StorageT {
    unStorageT :: ReaderT (Dynamic t (DMap k Identity)) (EventWriterT t (StorageMonoid k) m) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadHold t,
              MonadSample t, PostBuild t, MonadReflexCreateTrigger t, TriggerEvent t, MonadAtomicRef)

instance (Reflex t, GCompare k, Monad m) => HasStorage t k (StorageT t k m) where
  askStorage    = StorageT ask
  tellStorage e = StorageT . lift $ tellEvent e

instance MonadTrans (StorageT t k) where
  lift = StorageT . lift . lift

-- instance Requester t m => Requester t (StorageT t k m) where
--   type Request (StorageT t k m) = Request m
--   type Response (StorageT t k m) = Response m
--   {-# INLINABLE requesting #-}
--   requesting = lift . requesting
--   {-# INLINABLE requesting_ #-}
--   requesting_ = lift . requesting_

-- instance (Monad m, HasStorage t k m) => HasStorage t k (RequesterT t request response m) where
--   askStorage = lift askStorage
--   tellStorage e = RequesterT $ _

instance PerformEvent t m => PerformEvent t (StorageT t k m) where
  type Performable (StorageT t k m) = Performable m
  {-# INLINABLE performEvent_ #-}
  performEvent_ = lift . performEvent_
  {-# INLINABLE performEvent #-}
  performEvent = lift . performEvent

instance (DomBuilder t m, MonadHold t m, MonadFix m, GCompare k) => DomBuilder t (StorageT t k m) where
  type DomBuilderSpace (StorageT t k m) = DomBuilderSpace m
  textNode = lift . textNode
  element elementTag cfg (StorageT child) = StorageT $ element elementTag cfg child
  inputElement = lift . inputElement
  textAreaElement = lift . textAreaElement
  selectElement cfg (StorageT child) = StorageT $ selectElement cfg child
  placeRawElement = lift . placeRawElement
  wrapRawElement e = lift . wrapRawElement e

instance (Monad m, NotReady t m) => NotReady t (StorageT t k m)

instance (Adjustable t m, MonadHold t m, GCompare k) => Adjustable t (StorageT t k m) where
  runWithReplace a0 a' = StorageT $ runWithReplace (unStorageT a0) (fmapCheap unStorageT a')
  traverseDMapWithKeyWithAdjust f dm edm = StorageT $ traverseDMapWithKeyWithAdjust (\k v -> unStorageT $ f k v) (coerce dm) (coerceEvent edm)
  {-# INLINABLE traverseDMapWithKeyWithAdjust #-}
  traverseIntMapWithKeyWithAdjust f dm edm = StorageT $ traverseIntMapWithKeyWithAdjust (\k v -> unStorageT $ f k v) (coerce dm) (coerceEvent edm)
  {-# INLINABLE traverseIntMapWithKeyWithAdjust #-}
  traverseDMapWithKeyWithAdjustWithMove f dm edm = StorageT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> unStorageT $ f k v) (coerce dm) (coerceEvent edm)
  {-# INLINABLE traverseDMapWithKeyWithAdjustWithMove #-}

instance HasDocument m => HasDocument (StorageT t k m)

instance HasJSContext m => HasJSContext (StorageT t k m) where
  type JSContextPhantom (StorageT t k m) = JSContextPhantom m
  askJSContext = StorageT askJSContext
#ifndef ghcjs_HOST_OS
instance MonadJSM m => MonadJSM (StorageT t k m)
#endif

instance MonadRef m => MonadRef (StorageT t k m) where
  type Ref (StorageT t k m) = Ref m
  {-# INLINABLE newRef #-}
  newRef = lift . newRef
  {-# INLINABLE readRef #-}
  readRef = lift . readRef
  {-# INLINABLE writeRef #-}
  writeRef r = lift . writeRef r

runPureStorageT :: ( Reflex t
                   , Monad m
                   , MonadFix m
                   , MonadHold t m
                   , GCompare k
                   )
                => StorageT t k m a
                -> m a
runPureStorageT s = mdo
  (a, eChanges) <- runEventWriterT . flip runReaderT d . unStorageT $ s
  d <- foldDyn ($) DMap.empty $ storageMonoidToEndo <$> eChanges
  pure a

runStorageT :: forall t k m a.
               ( Reflex t
               , Monad m
               , MonadJSM m
               , MonadFix m
               , MonadHold t m
               , TriggerEvent t m
               , PerformEvent t m
               , PostBuild t m
               , MonadJSM (Performable m)
               , GKey k
               , GCompare k
               , ToJSONTag k Identity
               , FromJSONTag k Identity
               )
            => StorageType
            -> StorageT t k m a
            -> m a
runStorageT st s = mdo
  (a, eAppChanges) <- runEventWriterT . flip runReaderT d . unStorageT $ s

  eAppChanges' <- performEvent $ writeToStorage st <$> eAppChanges

  window <- currentWindowUnchecked
  eWindowChanges <- wrapDomEvent window (`on` storage) $ handleStorageEvents (Proxy :: Proxy k) st

  iStorage <- readFromStorage (Proxy :: Proxy k) st

  let
    eChanges = eWindowChanges <> eAppChanges'

  d <- foldDyn ($) iStorage $ storageMonoidToEndo <$> eChanges

  pure a

getStorage :: MonadJSM m
             => StorageType
             -> m Storage
getStorage LocalStorage =
  currentWindowUnchecked >>= getLocalStorage
getStorage SessionStorage =
  currentWindowUnchecked >>= getSessionStorage

sStore :: (MonadJSM m, GKey k, ToJSONTag k Identity)
       => StorageType
       -> k a
       -> Identity a
       -> m ()
sStore st k v = do
  s <- getStorage st
  setItem s (toKey (This k)) (decodeUtf8 . toStrict . encodeTagged k $ v)

sLoad :: (MonadJSM m, GKey k, FromJSONTag k Identity)
      => StorageType
      -> k a
      -> m (Maybe (Identity a))
sLoad st k = do
  s <- getStorage st
  mt <- getItem s (toKey (This k))
  pure $ decodeTagged k . fromStrict . encodeUtf8 =<< mt

sRemove :: (MonadJSM m, GKey k)
        => StorageType
        -> Some k
        -> m ()
sRemove st k = do
  s <- getStorage st
  removeItem s (toKey k)

writeToStorage :: ( Monad m
                  , MonadJSM m
                  , GKey k
                  , ToJSONTag k Identity
                  )
               => StorageType
               -> StorageMonoid k
               -> m (StorageMonoid k)
writeToStorage st sm = do
  void . DMap.traverseWithKey (\k v -> v <$ sStore st k v) . smInserts $ sm
  traverse_ (sRemove st) . Set.toList . smRemoves $ sm
  pure sm

readFromStorage :: ( Monad m
                   , MonadJSM m
                   , GKey k
                   , GCompare k
                   , FromJSONTag k Identity
                   )
                => Proxy k
                -> StorageType
                -> m (DMap k Identity)
readFromStorage p st = do
  let
    readKey (This k) = do
      mt <- sLoad st k
      pure $ (k DMap.:=>) <$> mt

  xs <- traverse readKey (keys p)
  pure . DMap.fromList . catMaybes $ xs

handleStorageEvents :: ( GKey k
                       , GCompare k
                       , FromJSONTag k Identity
                       )
                    => Proxy k
                    -> StorageType
                    -> EventM Window StorageEvent (StorageMonoid k)
-- TODO check the storage type
handleStorageEvents _ _{- st -} = do
  eS <- ask

  key :: Maybe Text <- getKey eS
  let
    s = fromKey =<< key
  case s of
    Nothing -> pure mempty
    Just (This k) -> do
      newValue :: Maybe Text <- getNewValue eS
      case newValue of
        Nothing ->
          pure $ smRemove k
        Just nv ->
          case decodeTagged k . fromStrict . encodeUtf8 $ nv of
            Just v -> pure $ smInsert k (runIdentity v)
            Nothing -> pure mempty

