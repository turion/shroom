module Data.Describe (module Data.Describe) where

-- base

import Data.Kind (Type)
import Data.Proxy (Proxy)

-- containers
import Data.Set (Set)
import Data.Set qualified as S

-- text
import Data.Text (Text)
import Data.Text qualified as T

-- aeson
import Data.Aeson (ToJSON, encode)

class (Bounded (Property a), Enum (Property a), ToJSON a) => Describe a where
  type Property a :: Type
  type Property a = ()

  describeType :: Proxy a -> Text

  failingProperties :: a -> Set (Property a)
  failingProperties = const S.empty

  describeProperties :: Proxy a -> Property a -> Text
  default describeProperties :: (Property a ~ ()) => Proxy a -> Property a -> Text
  describeProperties _ _ = "(None.)"

  examples :: Proxy a -> Set a
  examples _ = S.empty

description :: (Describe a) => Proxy a -> Text
description p =
  T.unlines $
    [ "Type description:"
    , describeType p
    , "Properties that should not fail:"
    ]
      <> (describeProperties p <$> [minBound .. maxBound])
      <> if S.null (examples p)
        then []
        else "Examples:" : (T.pack . show . encode <$> S.toList (examples p))
