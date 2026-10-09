{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

-- base
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
import Data.Universe.Class (Universe)

-- tasty
import Test.Tasty (defaultMain, testGroup)

-- tasty-golden
import Test.Tasty.Golden (goldenVsString)

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

main :: IO ()
main =
  defaultMain $
    testGroup
      "description"
      [ golden "Person" $ description (Proxy @Person)
      , golden "Email" $ description (Proxy @Email)
      , golden "Delivery" $ description (Proxy @Delivery)
      ]
  where
    golden name rendered =
      goldenVsString name ("test/golden/" <> name <> ".golden") $
        pure $
          encodeUtf8 $
            TL.fromStrict rendered
