{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

-- base
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)

-- containers
import Data.Set qualified as S

-- text
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding (encodeUtf8)

-- aeson
import Data.Aeson (ToJSON)

-- openapi3
import Data.OpenApi (ToSchema)

-- universe-base
import Data.Universe.Class (Universe, universe)

-- generics-sop
import Generics.SOP qualified as SOP

-- tasty
import Test.Tasty (defaultMain, testGroup)

-- tasty-golden
import Test.Tasty.Golden (goldenVsString)

-- tasty-hunit
import Test.Tasty.HUnit (assertBool, testCase)

-- shroom-class
import Control.Monad.Prompt.TH (deriveDescribable)
import Data.Shroom.Class

-- | A person with a name and an age.
data Person = Person
  { personName :: Text
  -- ^ The person's full name.
  , personAge :: Int
  -- ^ The person's age in years.
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, ToSchema)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

-- | A contact address that is an email address.
newtype Email = Email
  { emailAddress :: Text
  -- ^ The email address, which must contain an at sign.
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

-- | A property of an 'Email' that should hold.
data EmailProperty = EmailNotEmpty | EmailHasAtSign
  deriving (Bounded, Enum, Universe, Eq, Ord, Show)

-- | How a message should be delivered.
data Delivery
  = -- | Send the message by post.
    Post
  | -- | Send the message by email.
    ByEmail
  | -- | Do not send the message at all.
    Hold
  deriving (Eq, Ord, Show, Generic, ToJSON, ToSchema)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

-- | A piece of fruit.
data Fruit
  = -- | An apple.
    FruitApple
      { appleVariety :: Text
      -- ^ The variety of the apple.
      , appleSweetness :: Int
      -- ^ How sweet the apple is, from 1 to 10.
      }
  | -- | A banana, with its length in centimetres.
    FruitBanana Int
  | -- | A pear.
    FruitPear
      { pearRipe :: Bool
      -- ^ Whether the pear is ripe.
      }
  | FruitLemon
  deriving (Eq, Ord, Show, Generic, ToJSON, ToSchema)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

$(deriveDescribable ''Person)

instance Surveyable Person

$(deriveDescribable ''Email)

instance Surveyable Email where
  type Property Email = EmailProperty
  propertyHolds email EmailNotEmpty = not (T.null (emailAddress email))
  propertyHolds email EmailHasAtSign = T.elem '@' (emailAddress email)
  describeProperties _ EmailNotEmpty = Just "The email address is not empty."
  describeProperties _ EmailHasAtSign = Just "The email address must contain an '@' character."
  examples _ = S.fromList [Email "ada@example.org", Email "alan@example.com"]

$(deriveDescribable ''Delivery)

instance Surveyable Delivery

$(deriveDescribable ''Fruit)

instance Surveyable Fruit

-- | A name.
newtype Name = Name Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON)

-- | A property of a 'Name' that should hold; the second one is informal and undescribed.
data NameProperty = NameNotEmpty | NameIsReal
  deriving (Bounded, Enum, Universe, Eq, Ord, Show)

instance Describable Name where
  describeType _ = "A name."

instance Surveyable Name where
  type Property Name = NameProperty
  propertyHolds (Name n) NameNotEmpty = not (T.null n)
  propertyHolds _ NameIsReal = True
  describeProperties _ NameNotEmpty = Just "The name must not be empty."
  describeProperties _ NameIsReal = Nothing

-- | A hand-written product, surveyed structurally.
data Contact = Contact
  { contactName :: Name
  , contactEmail :: Email
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, SOP.Generic, SOP.HasDatatypeInfo)

instance Describable Contact where
  describeType _ = "A contact."

instance Surveyable Contact where
  type Property Contact = Structural Contact
  propertyHolds = structuralHolds
  describeProperties = describeStructural

-- | A hand-written product with positional fields.
data Pair = Pair Name Email
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, SOP.Generic, SOP.HasDatatypeInfo)

instance Describable Pair where
  describeType _ = "A pair."

instance Surveyable Pair where
  type Property Pair = Structural Pair
  propertyHolds = structuralHolds
  describeProperties = describeStructural

-- | A hand-written sum: a record, a positional constructor and one without fields.
data Parcel
  = Letter
      { letterTo :: Email
      , letterFrom :: Name
      }
  | Crate Name
  | Blank
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, SOP.Generic, SOP.HasDatatypeInfo)

instance Describable Parcel where
  describeType _ = "A parcel."

instance Surveyable Parcel where
  type Property Parcel = Structural Parcel
  propertyHolds = structuralHolds
  describeProperties = describeStructural

{- | Every structural property of a type, with its description, and for each
given value whether each property holds.
-}
structuralReport ::
  forall a.
  (Show a, Show (Structural a), SOP.Generic a, SOP.HasDatatypeInfo a, SOP.All2 Surveyable (SOP.Code a)) =>
  Proxy a ->
  [a] ->
  Text
structuralReport p values =
  T.unlines $
    ["universe:"]
      <> [ "  " <> T.pack (show prop) <> " | " <> fromMaybe "(undescribed)" (describeStructural p prop)
         | prop <- universe @(Structural a)
         ]
      <> concat
        [ ("value " <> T.pack (show v) <> ":")
            : [ "  " <> T.pack (show prop) <> ": " <> if structuralHolds v prop then "holds" else "violated"
              | prop <- universe @(Structural a)
              ]
        | v <- values
        ]

main :: IO ()
main =
  defaultMain $
    testGroup
      "description"
      [ golden "Person" $ description (Proxy @Person)
      , golden "Email" $ description (Proxy @Email)
      , golden "Delivery" $ description (Proxy @Delivery)
      , golden "Fruit" $ description (Proxy @Fruit)
      , golden "StructuralContact" $
          structuralReport
            (Proxy @Contact)
            [Contact (Name "Ada") (Email "ada@example.org"), Contact (Name "") (Email "ada@example.org"), Contact (Name "Ada") (Email "")]
      , golden "StructuralPair" $
          structuralReport
            (Proxy @Pair)
            [Pair (Name "") (Email "no-at-sign"), Pair (Name "Ada") (Email "ada@example.org")]
      , golden "StructuralParcel" $
          structuralReport
            (Proxy @Parcel)
            [Letter (Email "") (Name "Ada"), Crate (Name ""), Crate (Name "Ada"), Blank]
      , testGroup
          "base types"
          [ contains "Text" (description (Proxy @Text)) "A piece of text."
          , contains "[Int]" (description (Proxy @[Int])) "A list of values of the following type: A whole number."
          , contains "NonEmpty Bool" (description (Proxy @(NonEmpty Bool))) "A truth value: true or false."
          , contains "Maybe Text" (description (Proxy @(Maybe Text))) "A piece of text."
          , contains "[Maybe Int]" (describeType (Proxy @[Maybe Int])) "A whole number."
          ]
      ]
  where
    contains name rendered needle =
      testCase name $
        assertBool ("expected " <> show needle <> " in " <> show rendered) (needle `T.isInfixOf` rendered)
    golden name rendered =
      goldenVsString name ("test/golden/" <> name <> ".golden") $
        pure $
          encodeUtf8 $
            TL.fromStrict rendered
