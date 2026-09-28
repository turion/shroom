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

import Control.Monad.Prompt.Promptable (Promptable)
import Control.Monad.Prompt.TH (deriveDescribable)
import Data.Shroom.Class

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
data UserProperty = UserEmailNotEmpty | UserEmailHasAtSign
  deriving (Bounded, Enum, Universe, Eq, Ord, Show)

$(deriveDescribable ''User)

instance Surveyable User where
  type Property User = UserProperty
  describeProperties _ UserEmailNotEmpty = Just "The email address is not empty."
  describeProperties _ UserEmailHasAtSign = Just "The email address must contain an '@' character."
  propertyHolds user UserEmailNotEmpty = not (T.null (userEmail user))
  propertyHolds user UserEmailHasAtSign = T.elem '@' (userEmail user)

instance Promptable User

$(deriveDescribable ''Counter)

instance Surveyable Counter where
  examples _ = S.singleton (Counter 0)

instance Promptable Counter

$(deriveDescribable ''Coordinate)

instance Surveyable Coordinate

instance Promptable Coordinate
