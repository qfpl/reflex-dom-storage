{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.GADT.Aeson (
    GKey(..)
  , ToJSONTag(..)
  , FromJSONTag(..)
  , JSONDMap
  , encodeTagged
  , decodeTagged
  ) where

import Control.Applicative ((<|>))
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy(..))

import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.ByteString.Lazy (toStrict,  fromStrict)

import Data.Dependent.Sum (DSum(..))
import Data.Dependent.Map (DMap, Some(..))
import qualified Data.Dependent.Map as DMap
import Data.GADT.Compare (GCompare)

import Data.Aeson
import Data.Aeson.Encoding
import Data.Aeson.Parser hiding (value)
import Data.Aeson.Parser.Internal (jsonEOF)
import Data.Aeson.Types

class GKey k where
  toKey :: Some k -> Text
  fromKey :: Text -> Maybe (Some k)
  keys :: Proxy k -> [Some k]

class ToJSONTag k f where
  toJSONTagged :: k a -> f a -> Value

encodeTagged :: ToJSONTag k f => k a -> f a -> Text
encodeTagged k v =
  decodeUtf8 . toStrict . encodingToLazyByteString . value $ toJSONTagged k v

class FromJSONTag k f where
  parseJSONTagged :: k a -> Value -> Parser (f a)

decodeTagged :: FromJSONTag k f => k a -> Text -> Maybe (f a)
decodeTagged k =
    decodeWith jsonEOF (parse (parseJSONTagged k)) . fromStrict . encodeUtf8

type JSONDMap k f = DMap k f

instance (GKey k, ToJSONTag k f) => ToJSON (JSONDMap k f) where
  toJSON dm =
    let
      toPair (k :=> v) = (toKey (This k), toJSONTagged k v)
    in
      object . fmap toPair . DMap.toList $ dm

instance (GCompare k, GKey k, FromJSONTag k f) => FromJSON (JSONDMap k f) where
  parseJSON (Object v) =
    let
      ks = keys (Proxy :: Proxy k)
      maybeKey (This k) =
        (\x -> Just (k :=> x)) <$> explicitParseField (parseJSONTagged k) v (toKey (This k)) <|>
        pure Nothing
    in
      DMap.fromList . catMaybes <$> traverse maybeKey ks
  parseJSON x =
    typeMismatch "JSONDMap" x
