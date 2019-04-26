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
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
module Reflex.Dom.Storage.Base (
    StorageT(..)
  , StorageType(..)
  , runStorageT
  ) where

import Control.Monad (void)
import Data.Coerce (coerce)
import Data.Foldable (fold)
import Data.Functor.Identity (Identity(..))
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy(..))

import Control.Monad.Trans (MonadTrans, MonadIO, lift)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Primitive (PrimMonad(..))
import Control.Monad.Ref (MonadRef(..), MonadAtomicRef(..))
import Control.Monad.Reader (ReaderT, MonadReader(..), runReaderT, ask)
import Control.Monad.State (MonadState(..))
import Control.Monad.Exception (MonadException, MonadAsyncException)
import Control.Monad.Morph (hoist)

import Reflex
import Reflex.Host.Class
import Reflex.Dom.Core hiding (Value, Error, Window)
import Data.Functor.Misc (ComposeMaybe(..))

import Data.Text (Text)

import Data.Dependent.Map (DMap, Some(..), GCompare)
import qualified Data.Dependent.Map as DMap

import GHCJS.DOM (currentWindowUnchecked)
import GHCJS.DOM.Types (MonadJSM)
import GHCJS.DOM.EventM (EventM, on)
import GHCJS.DOM.Window (Window, getLocalStorage, getSessionStorage)
import GHCJS.DOM.WindowEventHandlers (storage)
import GHCJS.DOM.Storage (Storage(..), getItem, setItem, removeItem)
import GHCJS.DOM.StorageEvent

import Reflex.Dom.Builder.Immediate (wrapDomEvent)

import Reflex.Dom.Storage.Class

import Data.GADT.Aeson

data StorageType =
    SessionStorage
  | LocalStorage
  deriving (Eq, Ord, Show)

newtype StorageT t k m a =
  StorageT {
    unStorageT :: ReaderT (Incremental t (PatchDMap k Identity)) (EventWriterT t (PatchDMap k Identity) m) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadHold t,
              MonadException, MonadAsyncException, DomRenderHook t,
              MonadSample t, PostBuild t, MonadReflexCreateTrigger t, TriggerEvent t, MonadAtomicRef)

instance (Reflex t, Monad m, GCompare k) => HasStorage t k (StorageT t k m) where
  askStorage    = StorageT ask
  tellStorage e = StorageT $ tellEvent e

instance MonadTrans (StorageT t k) where
  lift = StorageT . lift . lift

instance Requester t m => Requester t (StorageT t k m) where
  type Request (StorageT t k m) = Request m
  type Response (StorageT t k m) = Response m
  requesting = lift . requesting
  requesting_ = lift . requesting_

instance (Adjustable t m, MonadHold t m, GCompare k) => Adjustable t (StorageT t k m) where
  runWithReplace a0 a' = StorageT $ runWithReplace (unStorageT a0) (fmapCheap unStorageT a')
  traverseDMapWithKeyWithAdjust f dm edm = StorageT $ traverseDMapWithKeyWithAdjust (\k v -> unStorageT $ f k v) (coerce dm) (coerceEvent edm)
  {-# INLINABLE traverseDMapWithKeyWithAdjust #-}
  traverseIntMapWithKeyWithAdjust f dm edm = StorageT $ traverseIntMapWithKeyWithAdjust (\k v -> unStorageT $ f k v) (coerce dm) (coerceEvent edm)
  {-# INLINABLE traverseIntMapWithKeyWithAdjust #-}
  traverseDMapWithKeyWithAdjustWithMove f dm edm = StorageT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> unStorageT $ f k v) (coerce dm) (coerceEvent edm)
  {-# INLINABLE traverseDMapWithKeyWithAdjustWithMove #-}

instance PerformEvent t m => PerformEvent t (StorageT t k m) where
  type Performable (StorageT t k m) = Performable m
  performEvent_ = lift . performEvent_
  performEvent = lift . performEvent

instance MonadRef m => MonadRef (StorageT t k m) where
  type Ref (StorageT t k m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

instance (MonadQuery t q m, Monad m) => MonadQuery t q (StorageT t k m) where
  tellQueryIncremental = lift . tellQueryIncremental
  askQueryResult = lift askQueryResult
  queryIncremental = lift . queryIncremental

instance (Monad m, NotReady t m) => NotReady t (StorageT t k m)

instance (DomBuilder t m, MonadHold t m, MonadFix m, GCompare k) => DomBuilder t (StorageT t k m) where
  type DomBuilderSpace (StorageT t k m) = DomBuilderSpace m
  textNode = lift . textNode
  element elementTag cfg (StorageT child) = StorageT $ element elementTag cfg child
  inputElement = lift . inputElement
  textAreaElement = lift . textAreaElement
  selectElement cfg (StorageT child) = StorageT $ selectElement cfg child
  placeRawElement = lift . placeRawElement
  wrapRawElement e = lift . wrapRawElement e

instance HasDocument m => HasDocument (StorageT t k m)

instance HasJSContext m => HasJSContext (StorageT t k m) where
  type JSContextPhantom (StorageT t k m) = JSContextPhantom m
  askJSContext = StorageT askJSContext
#ifndef ghcjs_HOST_OS
instance MonadJSM m => MonadJSM (StorageT t k m)
#endif

instance MonadReader r m => MonadReader r (StorageT t k m) where
  ask = lift ask
  local f (StorageT a) = StorageT . hoist (local f) $ a
  reader = lift . reader

instance MonadState s m => MonadState s (StorageT t k m) where
  get = lift get
  put = lift . put

instance EventWriter t w m => EventWriter t w (StorageT t k m) where
  tellEvent = lift . tellEvent

instance PrimMonad m => PrimMonad (StorageT t k m) where
  type PrimState (StorageT t k m) = PrimState m
  primitive = lift . primitive

instance (Prerender js t m, Reflex t, Monad m, GCompare k) => Prerender js t (StorageT t k m) where
  type Client (StorageT t k m) = StorageT t k (Client m)
  prerender server client = StorageT $ do
    env <- ask
    d <- lift . lift $ prerender (runEventWriterT . runReaderT (unStorageT server) $ env) (runEventWriterT . runReaderT (unStorageT client) $ env)
    let (a, r) = splitDynPure d
    lift . tellEvent $ switchPromptlyDyn r
    pure a

instance HasJS x m => HasJS x (StorageT t k m) where
  type JSX (StorageT t k m) = JSX m
  liftJS = lift . liftJS


{-
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
-}

runStorageT :: forall t js k m a.
               ( Reflex t
               , Monad m
               , MonadFix m
               , MonadHold t m
               , TriggerEvent t m
               , PerformEvent t m
               , PostBuild t m
               , GKey k
               , GCompare k
               , ToJSONTag k Identity
               , FromJSONTag k Identity
               , Prerender js t m
               )
            => StorageType
            -> StorageT t k m a
            -> m a
runStorageT st s = mdo
  stateDyn <- prerender (pure (never, DMap.empty)) $ do
    window <- currentWindowUnchecked
    eWindowChanges' <- wrapDomEvent window (`on` storage) $ handleStorageEvents (Proxy :: Proxy k) st
    iStorage' <- readFromStorage (Proxy :: Proxy k) st
    pure (eWindowChanges',iStorage')

  let eWindowChanges = switchDyn $ fst <$> stateDyn
  iStorage <- fmap snd . sample . current $ stateDyn
  i <- holdIncremental iStorage $ fold
    [ eWindowChanges
    , eAppChanges
    -- Once hydration switches over, patch the entire DMap with the value
    -- from local storage.
    , PatchDMap . DMap.map (ComposeMaybe . Just) . snd <$> updated stateDyn
    ]

  (a, eAppChanges) <- runEventWriterT . flip runReaderT i . unStorageT $ s
  prerender_ (pure ()) $ performEvent_ $ writeToStorage st <$> eAppChanges

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
  setItem s (toKey (This k)) (encodeTagged k v)

sLoad :: (MonadJSM m, GKey k, FromJSONTag k Identity)
      => StorageType
      -> k a
      -> m (Maybe (Identity a))
sLoad st k = do
  s <- getStorage st
  mt <- getItem s (toKey (This k))
  pure $ decodeTagged k =<< mt

sRemove :: (MonadJSM m, GKey k)
        => StorageType
        -> Some k
        -> m ()
sRemove st k = do
  s <- getStorage st
  removeItem s (toKey k)

writeToStorage :: forall m k.
                  ( Monad m
                  , MonadJSM m
                  , GKey k
                  , ToJSONTag k Identity
                  )
               => StorageType
               -> PatchDMap k Identity
               -> m ()
writeToStorage st pdm = do
  let
    change :: k a -> ComposeMaybe Identity a -> m (ComposeMaybe Identity a)
    change k v@(ComposeMaybe (Just a)) = v <$ sStore st k a
    change k v@(ComposeMaybe Nothing)  = v <$ sRemove st (This k)
  void . DMap.traverseWithKey change . unPatchDMap $ pdm

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
                    -> EventM Window StorageEvent (PatchDMap k Identity)
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
          pure . pdmRemove $ k
        Just nv ->
          case decodeTagged k nv of
            Just v -> pure . pdmInsert k . runIdentity $ v
            Nothing -> pure mempty
