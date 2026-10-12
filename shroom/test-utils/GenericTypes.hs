{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

{- | The two examples from the issues that asked for @deriving via Generically@
(a record, #21, and a sum, #22), written the way a user would write them:
shared by the integration cases of @shroom-baikai@, so a failure of the
strategy against a real provider has a minimal reproduction.

The composite types, 'Musician' and 'Fruit', derive 'Promptable' via
'Generically', so a prompt of one builds its parts one request at a time and
makes them cohere. Their leaves ('Name', 'City', 'Apple', 'Banana') are flat:
a single request each, as the guide in "Control.Monad.Prompt.Generic" advises.
-}
module GenericTypes (
  Musician (..),
  Name (..),
  NameProperty (..),
  City (..),
  Fruit (..),
  Apple (..),
  Banana (..),
  allPropertiesHold,
) where

-- base
import GHC.Generics (Generic, Generically (..))

-- text
import Data.Text (Text)
import Data.Text qualified as T

-- aeson
import Data.Aeson (FromJSON, ToJSON)

-- openapi3
import Data.OpenApi (ToSchema)

-- generics-sop
import Generics.SOP qualified as SOP

-- universe-base
import Data.Universe.Class (Universe, universe)

-- shroom
import Control.Monad.Prompt.Generic ()
import Control.Monad.Prompt.Promptable (Promptable)
import Control.Monad.Prompt.TH (deriveDescribable)
import Data.Shroom.Class

-- | A musician, with a name and the city they are from.
data Musician = Musician
  { name :: Name
  -- ^ The musician's full name.
  , city :: City
  -- ^ The city the musician is from.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, SOP.Generic, SOP.HasDatatypeInfo)

-- | The full name of a person.
newtype Name = Name Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, SOP.Generic, SOP.HasDatatypeInfo)

-- | The name of a city.
newtype City = City Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, SOP.Generic, SOP.HasDatatypeInfo)

-- | A fruit, either an apple or a banana.
data Fruit
  = -- | An apple.
    FruitApple Apple
  | -- | A banana.
    FruitBanana Banana
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, SOP.Generic, SOP.HasDatatypeInfo)

-- | An apple, counted by its kernels.
newtype Apple = Apple
  { kernels :: Int
  -- ^ The number of kernels in the apple.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, SOP.Generic, SOP.HasDatatypeInfo)

{- | A banana, measured by its length.

The field is called @lengthCm@ rather than @length@: the unit belongs in the
name, as the model sees it, and a field @length@ would clash with
'Prelude.length' in every module that imports this one unqualified.
-}
newtype Banana = Banana
  { lengthCm :: Double
  -- ^ The length of the banana, in centimetres.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, SOP.Generic, SOP.HasDatatypeInfo)

-- | A property of a name that should hold.
data NameProperty = NameNotEmpty
  deriving stock (Eq, Ord, Show, Bounded, Enum)
  deriving anyclass (Universe)

$(deriveDescribable ''Musician)

$(deriveDescribable ''Name)

$(deriveDescribable ''City)

$(deriveDescribable ''Fruit)

$(deriveDescribable ''Apple)

$(deriveDescribable ''Banana)

-- A property for the structural default of 'Musician' to find.
instance Surveyable Name where
  type Property Name = NameProperty
  describeProperties _ NameNotEmpty = Just "The name is not empty."
  propertyHolds (Name t) NameNotEmpty = not (T.null t)

instance Surveyable City

instance Surveyable Apple

instance Surveyable Banana

-- No @Property@, no methods: the properties of the fields, 'Name''s included.
instance Surveyable Musician

instance Surveyable Fruit

-- Leaves are flat: a single request each.
instance Promptable Name

instance Promptable City

instance Promptable Apple

instance Promptable Banana

-- Composites are prompted part by part, see "Control.Monad.Prompt.Generic".
deriving via Generically Musician instance Promptable Musician

deriving via Generically Fruit instance Promptable Fruit

-- | Whether every property of a value holds.
allPropertiesHold :: (Surveyable a) => a -> Bool
allPropertiesHold x = all (propertyHolds x) universe
