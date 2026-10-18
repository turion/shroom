{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

-- base
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Typeable (typeRep)
import Data.Void (Void)
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
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

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
  deriving anyclass (ToJSON, ToSchema)

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

-- | A hand-written product, surveyed structurally by the default.
data Contact = Contact
  { contactName :: Name
  , contactEmail :: Email
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, SOP.Generic, SOP.HasDatatypeInfo)

instance Describable Contact where
  describeType _ = "A contact."

instance Surveyable Contact

-- | A hand-written product with positional fields, surveyed with the structural property written out by hand.
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

-- | A registration, which combines a property of its own with those of its fields.
data Registration = Registration
  { registrationName :: Name
  , registrationEmail :: Email
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, ToSchema, SOP.Generic, SOP.HasDatatypeInfo)

-- | A property of a whole 'Registration' that should hold.
data RegistrationProperty = RegistrationEmailMentionsName
  deriving (Bounded, Enum, Universe, Eq, Ord, Show)

instance Describable Registration where
  describeType _ = "A registration."

instance Surveyable Registration where
  type Property Registration = Either RegistrationProperty (Structural Registration)
  propertyHolds (Registration (Name n) (Email e)) (Left RegistrationEmailMentionsName) = T.toLower n `T.isInfixOf` T.toLower e
  propertyHolds r (Right p) = structuralHolds r p
  describeProperties _ (Left RegistrationEmailMentionsName) = Just "The email address must contain the registrant's name."
  describeProperties px (Right p) = describeStructural px p

-- * Reachable types

-- | A street: the leaf of the reachable-types tree.
newtype Street = Street {streetName :: Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

-- | A record nesting a record.
data Address = Address {addressStreet :: Street, addressCity :: Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

-- | An enum.
data Mood = Happy | Grumpy
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

-- | A sum whose constructors refer to types that are also reachable another way.
data Shape = Circle Street | Square Address
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

-- | A recursive type.
data Tree = Leaf Int | Node [Tree]
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

-- | A record that reaches all of the above, some of them twice, and base types.
data Household = Household
  { householdAddress :: Address
  , householdShapes :: [Shape]
  , householdMood :: Maybe Mood
  , householdTree :: Tree
  , householdNote :: Text
  , householdSize :: Int
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

instance Describable Street where describeType _ = "A street."

instance Surveyable Street where type Property Street = Void

instance Describable Address where
  describeType _ = "An address."
  referencedTypes _ = reach @Street <> reach @Text

instance Surveyable Address where
  type Property Address = Void
  examples _ = S.singleton (Address (Street "Main Street") "Hull")

-- No 'referencedTypes': a hand-written instance contributes no reachable types unless it says so.
instance Describable Mood where describeType _ = "A mood."

instance Surveyable Mood where type Property Mood = Void

instance Describable Shape where
  describeType _ = "A shape."
  referencedTypes _ = reach @Street <> reach @Address

instance Surveyable Shape where type Property Shape = Void

instance Describable Tree where
  describeType _ = "A tree."
  referencedTypes _ = reach @Int <> reach @[Tree]

instance Surveyable Tree where type Property Tree = Void

instance Describable Household where
  describeType _ = "A household."
  referencedTypes _ =
    reach @Address <> reach @[Shape] <> reach @(Maybe Mood) <> reach @Tree <> reach @Text <> reach @Int

instance Surveyable Household where type Property Household = Void

-- | The lines of a description from the reachable-types section on, empty if it has none.
reachableLines :: Text -> [Text]
reachableLines = dropWhile (/= "The types above refer to the following types:") . T.lines

-- | The name 'description' gives a reachable type in its section header.
typeName :: Described -> String
typeName (Described q) = show (typeRep q)

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
      , testGroup
          "reachable types"
          [ testCase "each reachable documented type is listed once, breadth first, without base types, containers or the requested type" $
              reachableLines (description (Proxy @Household))
                @?= [ "The types above refer to the following types:"
                    , ""
                    , "Address:"
                    , "An address."
                    , ""
                    , "Shape:"
                    , "A shape."
                    , ""
                    , "Mood:"
                    , "A mood."
                    , ""
                    , "Tree:"
                    , "A tree."
                    , ""
                    , "Street:"
                    , "A street."
                    ]
          , testCase "a record nesting a record lists the nested one" $
              reachableLines (description (Proxy @Address))
                @?= ["The types above refer to the following types:", "", "Street:", "A street."]
          , testCase "a recursive type terminates and is not listed under itself" $
              reachableLines (description (Proxy @Tree)) @?= []
          , testCase "a hand-written instance without referencedTypes contributes no reachable types" $
              reachableLines (description (Proxy @Mood)) @?= []
          , testCase "a list of a record, a Maybe of an enum and a base type reach what their element reaches" $ do
              map typeName (reach @[Shape]) @?= ["Shape"]
              map typeName (reach @(Maybe Mood)) @?= ["Mood"]
              map typeName (reach @[Maybe Text]) @?= []
          , testCase "the section comes after the examples" $ do
              let (before, after) = break (== "The types above refer to the following types:") (T.lines (description (Proxy @Address)))
              assertBool "the examples come before the section" ("Example valid JSON responses:" `elem` before)
              length after @?= 4
          ]
      , testGroup
          "custom property combined with the structural one"
          [ contains "own property" (description (Proxy @Registration)) "The email address must contain the registrant's name."
          , contains "name field property" (description (Proxy @Registration)) "Field registrationName: The name must not be empty."
          , contains "email field property" (description (Proxy @Registration)) "Field registrationEmail: The email address is not empty."
          , violations "a valid value violates nothing" 0 (Registration (Name "Ada") (Email "ada@example.org"))
          , violations "an own property violation is found" 1 (Registration (Name "Ada") (Email "bob@example.org"))
          , violations "a field property violation is found" 1 (Registration (Name "") (Email "ada@example.org"))
          ]
      ]
  where
    violations name expected v =
      testCase name $
        length (filter (not . propertyHolds v) (universe @(Property Registration))) @?= expected
    contains name rendered needle =
      testCase name $
        assertBool ("expected " <> show needle <> " in " <> show rendered) (needle `T.isInfixOf` rendered)
    golden name rendered =
      goldenVsString name ("test/golden/" <> name <> ".golden") $
        pure $
          encodeUtf8 $
            TL.fromStrict rendered
