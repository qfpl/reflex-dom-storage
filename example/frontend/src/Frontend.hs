{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
module Frontend where

import qualified Data.Text as T
import Reflex.Dom.Core

import Common.Api
import Static

import Control.Applicative (liftA2)
import Control.Monad (void)
import Data.Bool (bool)
import Data.Functor.Identity (Identity(..))
import Data.Maybe (fromMaybe)

import qualified Data.Text as Text
import qualified Data.Dependent.Map as DMap

import Reflex.Dom.Storage.Base
import Reflex.Dom.Storage.Class
import Storage.Example

frontend :: (StaticWidget x (), Widget x ())
frontend = (head', body)
  where
    head' = el "title" $ text "Testing storage"

body :: MonadWidget t m => m ()
body = do
  text "Testing storage"
  void . runStorageT LocalStorage $ do
    initializeTag Tag1 0
    counter

counter :: (MonadWidget t m, HasStorage t ExampleTag m) => m ()
counter = el "div" $ do
  dTag1 <- askStorageTagDef Tag1 0

  eAdd <- button "Add"
  eClear <- button "Clear"
  display dTag1

  let
    eChange = mergeWith (.) [
        succ <$ eAdd
      , const 0 <$ eClear
      ]

  tellStorageInsert Tag1 $ (&) <$> current dTag1 <@> eChange



