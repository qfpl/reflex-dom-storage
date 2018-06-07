{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Storage.Example where

import Data.Functor.Identity (Identity(..))
import GHC.Generics

import Data.Dependent.Map (Some(..))

import Data.Aeson (ToJSON(..), FromJSON(..))

import Storage

data Foo = Foo { bar :: Int, baz :: String }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Foo where
instance FromJSON Foo where

data ExampleTag a where
  Tag1 :: ExampleTag Bool
  Tag2 :: ExampleTag Foo

-- TODO add a GCompare instance

instance GKey ExampleTag where
  toKey (This Tag1) = "tag1"
  toKey (This Tag2) = "tag2"

  fromKey t =
    case t of
      "tag1" -> Just (This Tag1)
      "tag2" -> Just (This Tag2)
      _ -> Nothing

instance ToJSONTag ExampleTag Identity where
  toJSONTagged Tag1 (Identity x) = toJSON x
  toJSONTagged Tag2 (Identity x) = toJSON x

instance FromJSONTag ExampleTag Identity where
  parseJSONTagged Tag1 x = Identity <$> parseJSON x
  parseJSONTagged Tag2 x = Identity <$> parseJSON x



