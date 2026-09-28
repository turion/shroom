{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Types and prompt chain for a celebrity trivia scenario.

The chain has two steps:

1. Ask the LLM to produce a list of exactly 3 celebrities.
2. Pick one deterministically, then ask the LLM to look up a Wikipedia
   article via web search + web fetch and return one surprising trivia fact.
-}
module CelebrityTypes (
  CelebrityList (..),
  CelebrityListProperty (..),
  CelebrityFact (..),
  CelebrityFactProperty (..),
  celebrityChain,
) where

-- base
import GHC.Generics (Generic)

-- text
import Data.Text (Text)
import Data.Text qualified as T

-- aeson
import Data.Aeson (FromJSON, ToJSON)

-- openapi3
import Data.OpenApi (ToSchema)

-- universe-base
import Data.Universe.Class (Universe)

-- shroom
import Control.Monad.Prompt

import Control.Monad.Prompt.TH (deriveDescribable)
import Data.Shroom.Class

-- * Types

-- | A list of exactly 3 celebrity names.
newtype CelebrityList = CelebrityList
  { celebrities :: [Text]
  -- ^ Exactly 3 celebrity names (first and last name).
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | A property of a 'CelebrityList' that should hold.
data CelebrityListProperty = CelebrityListHasThree
  deriving (Bounded, Enum, Universe, Eq, Ord, Show)

-- | A single celebrity trivia fact.
data CelebrityFact = CelebrityFact
  { celebrity :: Text
  -- ^ The full name of the celebrity.
  , triviaFact :: Text
  -- ^ One surprising trivia fact about this celebrity.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | A property of a 'CelebrityFact' that should hold.
data CelebrityFactProperty
  = CelebrityFactNameNotEmpty
  | CelebrityFactTriviaNotEmpty
  deriving (Bounded, Enum, Universe, Eq, Ord, Show)

$(deriveDescribable ''CelebrityList)

instance Surveyable CelebrityList where
  type Property CelebrityList = CelebrityListProperty
  describeProperties _ CelebrityListHasThree = Just "The list must contain exactly 3 celebrity names."
  propertyHolds cl CelebrityListHasThree = length cl.celebrities == 3

instance Promptable CelebrityList

$(deriveDescribable ''CelebrityFact)

instance Surveyable CelebrityFact where
  type Property CelebrityFact = CelebrityFactProperty
  describeProperties _ CelebrityFactNameNotEmpty = Just "The celebrity name must not be empty."
  describeProperties _ CelebrityFactTriviaNotEmpty = Just "The trivia fact must not be empty."
  propertyHolds cf CelebrityFactNameNotEmpty = not (T.null cf.celebrity)
  propertyHolds cf CelebrityFactTriviaNotEmpty = not (T.null cf.triviaFact)

instance Promptable CelebrityFact

-- * Prompt chain

{- | A two-step prompt chain for celebrity trivia.

1. Generate a list of exactly 3 celebrities.
2. Pick one (deterministically), look up their Wikipedia page using
   @web_search@ and @web_fetch@, and return one surprising trivia fact.

Requires 'duckDuckGoSearchHandler', 'wikipediaSearchHandler', and 'webFetchHandler' as tools.
-}
celebrityChain :: (Monad m) => PromptT m CelebrityFact
celebrityChain = do
  context "You are a trivia assistant. You MUST use your tools to look up information — do not answer from memory."
  CelebrityList names <- prompt @CelebrityList
  let chosen = names !! (length names `mod` 3)
  promptWith @CelebrityFact $
    "You MUST use your tools to look up "
      <> chosen
      <> ". First search the web to find their Wikipedia page URL, then fetch that URL."
      <> " Do not answer from memory. Return one surprising trivia fact found in the fetched page."
