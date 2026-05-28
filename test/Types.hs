{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

-- base
import GHC.Generics (Generic)

-- text
import Data.Text (Text)
import Data.Text qualified as T

-- aeson
import Data.Aeson (FromJSON, ToJSON)

-- openapi3
import Data.OpenApi (ToSchema)

-- containers
import Data.Set qualified as S

-- universe-base
import Data.Universe.Class (Universe)

-- shroom

import Control.Monad.Prompt.TH (deriveDescribeType)
import Data.Describe

-- | A user with a name and an email address.
data User = User
  { userName :: Text
  -- ^ The user's full name.
  , userEmail :: Text
  -- ^ The user's email address.
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

-- | An integer counter that keeps track of how many times an event has occurred.
newtype Counter = Counter Int
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | A geographic coordinate expressed as a latitude\/longitude pair.
data Coordinate = Coordinate
  { latitude :: Double
  -- ^ The latitude in degrees.
  , longitude :: Double
  -- ^ The longitude in degrees.
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

-- | A property of a user that should hold.
data UserProperty = UserEmailNotEmpty
  deriving (Bounded, Enum, Universe, Eq, Ord, Show)

-- We need a new declaration group so TH can reify the types defined above.
$(pure [])

instance Describe User where
  type Property User = UserProperty
  describeType = $(deriveDescribeType ''User)
  describeProperties _ UserEmailNotEmpty = Just "The email address is not empty."
  propertyHolds user UserEmailNotEmpty = not (T.null (userEmail user))

instance Describe Counter where
  describeType = $(deriveDescribeType ''Counter)
  examples _ = S.singleton (Counter 0)

instance Describe Coordinate where
  describeType = $(deriveDescribeType ''Coordinate)
