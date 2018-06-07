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
module Storage where

import Data.Functor.Identity (Identity(..))
import Data.Monoid hiding ((<>))
import Data.Proxy (Proxy(..))
import Data.Semigroup

import Control.Monad.Trans (MonadTrans, MonadIO, lift)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Ref (MonadRef(..), MonadAtomicRef)
import Control.Monad.Exception (MonadAsyncException, MonadException)
import Control.Monad.Reader (ReaderT, runReaderT, ask)

import Reflex
import Reflex.Host.Class

import Data.Text (Text)
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Dependent.Map (DMap, Some(..), GCompare)
import qualified Data.Dependent.Map as DMap

import Data.Aeson (ToJSON, FromJSON, Value)
import Data.Aeson.Types (Parser)

import GHCJS.DOM (currentWindowUnchecked)
import GHCJS.DOM.Types (MonadJSM, liftJSM)
import GHCJS.DOM.EventM (EventM, on)
import GHCJS.DOM.Window (Window)
import GHCJS.DOM.WindowEventHandlers (storage)
import GHCJS.DOM.StorageEvent

import Reflex.Dom.Builder.Immediate (wrapDomEvent)

class GKey t where
  toKey :: Some t -> Text
  fromKey :: Text -> Maybe (Some t)

class ToJSONTag t f where
  toJSONTagged :: t a -> f a -> Value

class FromJSONTag t f where
  parseJSONTagged :: t a -> Value -> Parser (f a)

data StorageType =
    SessionStorage
  | LocalStorage
  deriving (Eq, Ord, Show)

data StorageMonoid k =
  StorageMonoid {
    smInserts :: DMap k Identity
  , smChanges :: DMap k Endo
  , smRemoves :: Set (Some k)
  }

instance GCompare k => Semigroup (StorageMonoid k) where
  (StorageMonoid i1 c1 r1) <> (StorageMonoid i2 c2 r2) =
    StorageMonoid (DMap.union i1 i2) (DMap.unionWithKey (const (<>)) c1 c2) (Set.union r1 r2)

instance GCompare k => Monoid (StorageMonoid k) where
  mempty = StorageMonoid DMap.empty DMap.empty Set.empty
  mappend = (<>)

smInsert :: GCompare k => k a -> a -> StorageMonoid k
smInsert k a = StorageMonoid (DMap.singleton k (Identity a)) DMap.empty mempty

smChange :: GCompare k => k a -> (a -> a) -> StorageMonoid k
smChange k f = StorageMonoid DMap.empty (DMap.singleton k (Endo f)) mempty

smRemove :: k a -> StorageMonoid k
smRemove k = StorageMonoid DMap.empty DMap.empty (Set.singleton (This k))

storageMonoidToEndo :: GCompare k
                    => StorageMonoid k
                    -> DMap k Identity
                    -> DMap k Identity
storageMonoidToEndo (StorageMonoid inserts changes removes) d =
  let
    idToConstEndo = Endo . const . runIdentity
    constEndoMap = DMap.map idToConstEndo d
    padChanges = DMap.union changes constEndoMap
    appId f = fmap (appEndo f)
    toRemove k = Set.member (This k) removes
  in
    DMap.filterWithKey (\k _ -> Set.notMember (This k) removes) .
    DMap.intersectionWithKey (const appId) padChanges .
    DMap.union inserts $
    d

newtype StorageT t k m a =
  StorageT {
    unStorageT :: ReaderT (Dynamic t (DMap k Identity)) (EventWriterT t (StorageMonoid k) m) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadHold t,
              MonadSample t, MonadAsyncException, MonadException, PostBuild t,
              MonadReflexCreateTrigger t, TriggerEvent t, MonadAtomicRef)

instance MonadTrans (StorageT t k) where
  lift = StorageT . lift . lift

class HasStorage t k m where
  askStorage  :: m (Dynamic t (DMap k Identity))
  tellStorage :: Event t (StorageMonoid k) -> m ()

instance (Reflex t, GCompare k, Monad m) => HasStorage t k (StorageT t k m) where
  askStorage    = StorageT ask
  tellStorage e = StorageT . lift $ tellEvent e

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

-- instance PerformEvent t m => PerformEvent t (StorageT t k m) where
--   type Performable (StorageT t k m) = Performable m
--   {-# INLINABLE performEvent_ #-}
--   performEvent_ = lift . performEvent_
--   {-# INLINABLE performEvent #-}
--   performEvent = lift . performEvent

instance MonadRef m => MonadRef (StorageT t k m) where
  type Ref (StorageT t k m) = Ref m
  {-# INLINABLE newRef #-}
  newRef = lift . newRef
  {-# INLINABLE readRef #-}
  readRef = lift . readRef
  {-# INLINABLE writeRef #-}
  writeRef r = lift . writeRef r

runStorageT :: forall t k m a.
               ( Reflex t
               , Monad m
               , MonadJSM m
               , MonadFix m
               , MonadHold t m
               , TriggerEvent t m
               , GKey k
               , GCompare k
               , ToJSONTag k Identity
               , FromJSONTag k Identity
               )
            => StorageType
            -> StorageT t k m a
            -> m a
runStorageT st s = mdo
  let
    ew = flip runReaderT d . unStorageT $ s
  (a, eAppChanges) <- runEventWriterT ew

  -- TODO make the eAppChanges happen in storage

  window <- currentWindowUnchecked
  eWindowChanges <- wrapDomEvent window (`on` storage) $ (storageHandler (Proxy :: Proxy k) st)

  let
    eChanges = eWindowChanges <> eAppChanges

  d <- foldDyn ($) DMap.empty $ storageMonoidToEndo <$> eChanges
  pure a

storageHandler :: GCompare k => Proxy k -> StorageType -> EventM Window StorageEvent (StorageMonoid k)
storageHandler _ st = do
  eS <- ask

  -- TODO gather the changes from the event

  pure mempty

tellStorageInsert :: (Reflex t, GCompare k, Monad m, HasStorage t k m)
                  => k a
                  -> Event t a
                  -> m ()
tellStorageInsert k e =
  tellStorage $ smInsert k <$> e

tellStorageChange :: (Reflex t, GCompare k, Monad m, HasStorage t k m)
                  => k a
                  -> Event t (a -> a)
                  -> m ()
tellStorageChange k e =
  tellStorage $ smChange k <$> e

tellStorageRemove :: (Reflex t, Monad m, HasStorage t k m)
                  => k a
                  -> Event t ()
                  -> m ()
tellStorageRemove k e =
  tellStorage $ smRemove k <$ e



