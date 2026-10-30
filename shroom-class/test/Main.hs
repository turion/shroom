{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

-- base
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, typeRep)
import Data.Void (Void)
import GHC.Generics (Generic, Generically (..))
import GHC.TypeLits (Symbol)

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

-- * Derived reachable types

-- | A colour of paint.
data Colour
  = -- | Like a post box.
    Red
  | -- | Like a lawn.
    Green
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

-- | Either a value was picked, or none was.
data Choice a
  = -- | The picked value.
    Pick a
  | -- | Nothing was picked.
    Decline
  deriving stock (Eq, Ord, Show, Generic)

-- The generic defaults need the context of the parameter, which only a standalone clause can state.
deriving anyclass instance (ToJSON a) => ToJSON (Choice a)

deriving anyclass instance (ToSchema a) => ToSchema (Choice a)

-- | The size of a tile.
data Dimensions = Dimensions
  { dimWidth :: Int
  -- ^ The width in millimetres.
  , dimHeight :: Int
  -- ^ The height in millimetres.
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

-- | A text printed on a tile.
newtype Label = Label
  { labelText :: Text
  -- ^ The words that are printed.
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

-- | How a tile is cut.
data Cut
  = -- | Straight edges, to the given dimensions.
    Straight Dimensions
  | -- | A round tile of the given diameter in millimetres.
    Curved Int
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

-- | A ceramic tile.
data Tile = Tile
  { tileCut :: Cut
  -- ^ How the tile is cut.
  , tileSize :: Dimensions
  -- ^ The size of the tile when laid.
  , tileLabels :: [Label]
  -- ^ The labels printed on the tile.
  , tileColour :: Choice Colour
  -- ^ The chosen colour, if any.
  , tileNote :: Text
  -- ^ A note for the tiler.
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

-- | A folder, holding files.
data Folder = Folder
  { folderName :: Text
  -- ^ The name of the folder.
  , folderFiles :: [File]
  -- ^ What is in the folder.
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

-- | A file, which is either some text or another folder.
data File
  = -- | Some text.
    Plain Text
  | -- | A folder in place of a file.
    Directory Folder
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

-- | An outline: a title with sub-outlines, a recursive type.
data Outline = Outline
  { outlineTitle :: Text
  -- ^ The title of the outline.
  , outlineChildren :: [Outline]
  -- ^ The sub-outlines.
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

-- | A span between points, a positional sum with a doc on each argument.
data Span
  = -- | Between two points.
    Between
      -- | The first point.
      Int
      -- | The second point.
      Int
  | -- | From a point on, with one argument documented and one not.
    Starting
      -- | The point to start at.
      Int
      Text
  | Whole
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

-- | A bound, in millimetres.
newtype Bounds
  = -- | The largest allowed value.
    Bounds Int
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

-- | A point, a single positional constructor with a doc on each argument.
data Point
  = -- | A point in the plane.
    Point
      -- | The horizontal position.
      Int
      -- | The vertical position.
      Int
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

-- The two layouts below are the ones fourmolu would rewrite into the leading form above, so it
-- is told to leave them alone.

{- FOURMOLU_DISABLE -}

-- | How a purchase is made, a sum in the one-line form.
data Purchase
  = ByFuel Int -- ^ Bought by the litre.
  | ByVoucher Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

-- | A couple, each argument on a line of its own with a trailing comment.
data Couple = Couple
  Int
  -- ^ The left one.
  Int
  -- ^ The right one.
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

{- FOURMOLU_ENABLE -}

-- | A wrapper whose constructor and field have no docs.
newtype Wrapped = Wrapped Int
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

-- | An amount in some unit, a phantom parameter of kind Symbol.
newtype Measure (unit :: Symbol) = Measure
  { measureAmount :: Double
  -- ^ The amount, in the unit.
  }
  deriving stock (Eq, Ord, Show, Generic)

deriving anyclass instance ToJSON (Measure unit)

deriving anyclass instance (Typeable unit) => ToSchema (Measure unit)

-- | A value tagged by an unannotated phantom parameter, which is poly-kinded under GHC2024.
newtype Tagged a = Tagged Double
  deriving stock (Eq, Ord, Show, Generic)

deriving anyclass instance ToJSON (Tagged a)

deriving anyclass instance (Typeable k, Typeable (a :: k)) => ToSchema (Tagged a)

-- The field types are derived before the types that have them as fields.
$(deriveDescribable ''Colour)

$(deriveDescribable ''Choice)

$(deriveDescribable ''Dimensions)

$(deriveDescribable ''Label)

$(deriveDescribable ''Cut)

$(deriveDescribable ''Tile)

instance Surveyable Tile where type Property Tile = Void

-- Mutually recursive types are derived by one splice.
$(concat <$> traverse deriveDescribable [''Folder, ''File])

instance Surveyable Folder where type Property Folder = Void

instance Surveyable File where type Property File = Void

-- A self-recursive type needs nothing special.
$(deriveDescribable ''Outline)

instance Surveyable Outline where type Property Outline = Void

-- The positional types, whose fields are all base types.
$(deriveDescribable ''Span)

instance Surveyable Span where type Property Span = Void

$(deriveDescribable ''Purchase)

$(deriveDescribable ''Bounds)

$(deriveDescribable ''Couple)

$(deriveDescribable ''Point)

instance Surveyable Point where type Property Point = Void

$(deriveDescribable ''Wrapped)

-- A phantom parameter that is not of kind Type needs no Describable, only Typeable.
$(deriveDescribable ''Measure)

instance (Typeable unit) => Surveyable (Measure unit) where type Property (Measure unit) = Void

$(deriveDescribable ''Tagged)

instance (Typeable k, Typeable (a :: k)) => Surveyable (Tagged a) where type Property (Tagged a) = Void

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
      , golden "Tile" $ description (Proxy @Tile)
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
          , contains "Set Bool" (description (Proxy @(S.Set Bool))) "A truth value: true or false."
          , contains "Int64" (description (Proxy @Int64)) "A whole number."
          , contains "Word" (description (Proxy @Word)) "A non-negative whole number."
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
          , testCase "a Set of an enum reaches the element's entry and gets no section of its own" $ do
              map typeName (reach @(S.Set Mood)) @?= ["Mood"]
              map typeName (reach @(S.Set Int64)) @?= []
              let sectionHeaders = filter (T.isSuffixOf ":") (reachableLines (description (Proxy @(S.Set Mood))))
              sectionHeaders @?= ["The types above refer to the following types:", "Mood:"]
          , testGroup
              "derived with deriveDescribable"
              [ testCase "every nested type is named once, breadth first: sum, record, list element, parameterised type and its argument" $
                  map typeName (reachableTypes (Proxy @Tile))
                    @?= ["Cut", "Dimensions", "Label", "Choice Colour", "Colour"]
              , testCase "Dimensions is reached through Cut and through a field of Tile, and has one section" $
                  length (filter (== "Dimensions:") (T.lines (description (Proxy @Tile)))) @?= 1
              , testCase "a parameterised type derives with a Describable context, and its argument is reachable" $ do
                  map typeName (reachableTypes (Proxy @(Choice Colour))) @?= ["Colour"]
                  map typeName (reachableTypes (Proxy @(Choice Dimensions))) @?= ["Dimensions"]
                  map typeName (reachableTypes (Proxy @(Choice Text))) @?= []
                  map typeName (reach @(Choice Colour)) @?= ["Choice Colour"]
              , testCase "mutually recursive types derived in one splice terminate, each listing the other once" $ do
                  reachableLines (description (Proxy @Folder))
                    @?= ["The types above refer to the following types:", "", "File:", "A file, which is either some text or another folder.", "- Plain: Some text.", "- Directory: A folder in place of a file.", ""]
                  map typeName (reachableTypes (Proxy @File)) @?= ["Folder"]
              , testCase "a self-recursive derived type is not listed under itself" $
                  reachableLines (description (Proxy @Outline)) @?= []
              , testCase "a Symbol-kinded phantom parameter derives, and its description renders at a concrete unit" $ do
                  describeType (Proxy @(Measure "metre"))
                    @?= T.unlines ["An amount in some unit, a phantom parameter of kind Symbol.", "- measureAmount: The amount, in the unit."]
                  assertBool "the description names the type" ("An amount in some unit" `T.isInfixOf` description (Proxy @(Measure "metre")))
                  reachableLines (description (Proxy @(Measure "second"))) @?= []
                  assertBool "two units are two types" (describedIdentity (Proxy @(Measure "metre")) /= describedIdentity (Proxy @(Measure "second")))
              , testCase "an unannotated, poly-kinded phantom parameter derives, and its description renders at concrete instantiations" $ do
                  describeType (Proxy @(Tagged Int))
                    @?= T.unlines ["A value tagged by an unannotated phantom parameter, which is poly-kinded under GHC2024."]
                  assertBool "the description names the type" ("unannotated phantom parameter" `T.isInfixOf` description (Proxy @(Tagged Int)))
                  assertBool "the description renders at a parameter of another kind" ("unannotated phantom parameter" `T.isInfixOf` description (Proxy @(Tagged "label")))
                  assertBool "the description renders at a parameter of kind Type -> Type" ("unannotated phantom parameter" `T.isInfixOf` description (Proxy @(Tagged Maybe)))
              ]
          , testGroup
              "through Generically"
              [ testCase "a recursive type promoted through Generically is not listed under itself" $
                  map typeName (reachableTypes (Proxy @(Generically Outline))) @?= []
              , testCase "the wrapper reaches what the wrapped type reaches" $
                  map typeName (reachableTypes (Proxy @(Generically Folder))) @?= ["File"]
              , testCase "a mutually recursive type through Generically lists the other type, not itself" $
                  map typeName (reachableTypes (Proxy @(Generically File))) @?= ["Folder"]
              , testCase "the wrapper has the identity of the wrapped type" $
                  describedIdentity (Proxy @(Generically Outline)) @?= describedIdentity (Proxy @Outline)
              ]
          , testCase "the section comes after the examples" $ do
              let (before, after) = break (== "The types above refer to the following types:") (T.lines (description (Proxy @Address)))
              assertBool "the examples come before the section" ("Example valid JSON responses:" `elem` before)
              length after @?= 4
          ]
      , golden "Span" $ description (Proxy @Span)
      , testGroup
          "positional constructor docs"
          [ testCase "a sum with a doc on each argument renders each documented argument under its constructor, by position" $
              describeType (Proxy @Span)
                @?= T.unlines
                  [ "A span between points, a positional sum with a doc on each argument."
                  , "- Between: Between two points."
                  , "  - field 1: The first point."
                  , "  - field 2: The second point."
                  , "- Starting: From a point on, with one argument documented and one not."
                  , "  - field 1: The point to start at."
                  , "- Whole"
                  ]
          , testCase "a sum in the one-line form documents the constructor, as before" $
              describeType (Proxy @Purchase)
                @?= T.unlines
                  [ "How a purchase is made, a sum in the one-line form."
                  , "- ByFuel: Bought by the litre."
                  , "- ByVoucher"
                  ]
          , testCase "a single constructor's own doc is rendered after the type's" $
              describeType (Proxy @Bounds)
                @?= T.unlines ["A bound, in millimetres.", "The largest allowed value."]
          , testCase "a single positional constructor lists its documented arguments directly" $
              describeType (Proxy @Point)
                @?= T.unlines
                  [ "A point, a single positional constructor with a doc on each argument."
                  , "A point in the plane."
                  , "- field 1: The horizontal position."
                  , "- field 2: The vertical position."
                  ]
          , testCase "a single positional constructor in the trailing-comment layout lists its arguments the same way" $
              describeType (Proxy @Couple)
                @?= T.unlines
                  [ "A couple, each argument on a line of its own with a trailing comment."
                  , "- field 1: The left one."
                  , "- field 2: The right one."
                  ]
          , testCase "annotateFieldTypes leaves a positional argument line alone" $ do
              let rendered = T.lines (description (Proxy @Point))
              assertBool "the first argument line is unchanged" ("- field 1: The horizontal position." `elem` rendered)
              assertBool "the second argument line is unchanged" ("- field 2: The vertical position." `elem` rendered)
          , testCase "undocumented positional fields add no lines" $
              describeType (Proxy @Wrapped) @?= "A wrapper whose constructor and field have no docs.\n"
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
      , testGroup
          "PropertyCheck"
          [ testCase "description lists the described properties in universe order and leaves the undescribed one out" $ do
              let propertySection = takeWhile (T.isPrefixOf "- ") $ drop 1 $ dropWhile (/= "The following properties MUST hold in your response:") $ T.lines $ description (Proxy @Registration)
              propertySection
                @?= [ "- The email address must contain the registrant's name."
                    , "- Field registrationName: The name must not be empty."
                    , "- Field registrationEmail: The email address is not empty."
                    , "- Field registrationEmail: The email address must contain an '@' character."
                    ]
          , testCase "typeProperties has one entry per element of the universe, described and checked like it" $ do
              let checks = typeProperties @Registration
                  values = [Registration (Name "Ada") (Email "ada@example.org"), Registration (Name "") (Email "ada@example.org"), Registration (Name "Ada") (Email "bob")]
              length checks @?= length (universe @(Property Registration))
              length checks @?= 5
              [c.description | c <- checks] @?= [describeProperties (Proxy @Registration) p | p <- universe]
              mapM_ (\v -> [c.holds v | c <- checks] @?= [propertyHolds v p | p <- universe]) values
          , testCase "propertyLines gives a line per described property, in order, and none for an undescribed one" $ do
              propertyLines [PropertyCheck {description = Just "The number should be even", holds = even}, PropertyCheck Nothing (const True), PropertyCheck (Just "The number should be positive") (> (0 :: Int))]
                @?= ["- The number should be even", "- The number should be positive"]
              propertyLines ([] :: [PropertyCheck Int]) @?= []
          , testCase "PropertyCheck is built by record syntax next to the description function" $ do
              let c = PropertyCheck {description = Just "The number should be even", holds = even} :: PropertyCheck Int
              c.description @?= Just "The number should be even"
              assertBool "even 4 holds" (c.holds 4)
              assertBool "even 3 does not hold" (not (c.holds 3))
              assertBool "the description function is still callable" ("A whole number." `T.isInfixOf` description (Proxy @Int64))
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
