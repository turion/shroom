{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Types for a multi-step conference scheduling scenario.

The prompt chain has four steps, each depending on the previous:

1. 'Speaker' — who is presenting?
2. 'Talk' — what are they presenting? (references the speaker)
3. 'Session' — what session contains the talk? (contains 'Talk' values)
4. 'ConferenceSchedule' — the full day's schedule (contains 'Session' values)
-}
module ConferenceTypes (
  Speaker (..),
  SpeakerProperty (..),
  Speakers (..),
  SpeakersProperty (..),
  Talk (..),
  TalkProperty (..),
  Slot (..),
  SlotProperty (..),
  ConferenceSchedule (..),
  ScheduleProperty (..),
  conferenceChain,
) where

-- base
import Data.List (nub)
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
import Control.Monad.Prompt.TH (deriveDescribeType)
import Data.Describe

-- * Types

-- | A speaker at an academic conference, identified by name, institution, and a short bio.
data Speaker = Speaker
  { speakerName :: Text
  -- ^ The speaker's full name, e.g. "Ada Lovelace".
  , speakerAffiliation :: Text
  -- ^ The speaker's institutional affiliation, e.g. "University of Cambridge".
  , speakerBio :: Text
  -- ^ A one-paragraph biography of the speaker.
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

-- | A property of a speaker that should hold.
data SpeakerProperty
  = SpeakerNameNotEmpty
  | SpeakerAffiliationNotEmpty
  deriving (Bounded, Enum, Universe, Eq, Ord, Show)

-- | A list of speakers at the conference. Must contain between 3 and 10 speakers.
newtype Speakers = Speakers
  { speakers :: [Speaker]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | A property of a speakers list that should hold.
data SpeakersProperty = SpeakersBetween3And10
  deriving (Bounded, Enum, Universe, Eq, Ord, Show)

-- | A talk submitted to a conference session, with title, abstract, and the presenting speaker's name.
data Talk = Talk
  { talkTitle :: Text
  -- ^ The title of the talk.
  , talkAbstract :: Text
  -- ^ A brief abstract describing the talk's content (1-3 sentences).
  , talkSpeakerName :: Text
  -- ^ The full name of the presenting speaker. Must exactly match a known speaker's name.
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

-- | A property of a talk that should hold.
data TalkProperty
  = TalkTitleNotEmpty
  | TalkSpeakerNameNotEmpty
  deriving (Bounded, Enum, Universe, Eq, Ord, Show)

-- | A timetable slot at the conference, containing a single talk with its start and end time.
data Slot = Slot
  { slotStart :: Text
  -- ^ The start time of the slot, e.g. "09:00".
  , slotEnd :: Text
  -- ^ The end time of the slot, e.g. "09:45".
  , slotTalk :: Talk
  -- ^ The talk presented in this slot.
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

-- | A property of a slot that should hold.
data SlotProperty
  = SlotStartNotEmpty
  | SlotEndNotEmpty
  deriving (Bounded, Enum, Universe, Eq, Ord, Show)

-- | A full day's schedule for the conference, listing all timetable slots in order.
data ConferenceSchedule = ConferenceSchedule
  { scheduleDay :: Text
  -- ^ The day of the week for this schedule, e.g. "Monday".
  , scheduleSlots :: [Slot]
  {- ^ The ordered list of timetable slots for the day. Must contain at least one slot,
    one per speaker, and slots must be contiguous (each slot's end time equals the next slot's start time).
  -}
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

-- | A property of a schedule that should hold.
data ScheduleProperty
  = ScheduleHasAtLeastOneSlot
  | ScheduleSlotsAreContiguous
  | ScheduleEachTalkOccursOnce
  deriving (Bounded, Enum, Universe, Eq, Ord, Show)

-- We need a new declaration group so TH can reify the types defined above.
$(pure [])

instance Describe Speakers where
  type Property Speakers = SpeakersProperty
  describeType = $(deriveDescribeType ''Speakers)
  describeProperties _ SpeakersBetween3And10 = Just "The list must contain between 3 and 10 speakers (inclusive)."
  propertyHolds s SpeakersBetween3And10 = let n = length (speakers s) in n >= 3 && n <= 10

instance Describe Speaker where
  type Property Speaker = SpeakerProperty
  describeType = $(deriveDescribeType ''Speaker)
  describeProperties _ SpeakerNameNotEmpty = Just "The speaker's name must not be empty."
  describeProperties _ SpeakerAffiliationNotEmpty = Just "The speaker's affiliation must not be empty."
  propertyHolds s SpeakerNameNotEmpty = not (T.null (speakerName s))
  propertyHolds s SpeakerAffiliationNotEmpty = not (T.null (speakerAffiliation s))

instance Describe Talk where
  type Property Talk = TalkProperty
  describeType = $(deriveDescribeType ''Talk)
  describeProperties _ TalkTitleNotEmpty = Just "The talk title must not be empty."
  describeProperties _ TalkSpeakerNameNotEmpty = Just "The speaker name must not be empty."
  propertyHolds t TalkTitleNotEmpty = not (T.null (talkTitle t))
  propertyHolds t TalkSpeakerNameNotEmpty = not (T.null (talkSpeakerName t))

instance Describe Slot where
  type Property Slot = SlotProperty
  describeType = $(deriveDescribeType ''Slot)
  describeProperties _ SlotStartNotEmpty = Just "The slot start time must not be empty."
  describeProperties _ SlotEndNotEmpty = Just "The slot end time must not be empty."
  propertyHolds s SlotStartNotEmpty = not (T.null (slotStart s))
  propertyHolds s SlotEndNotEmpty = not (T.null (slotEnd s))

instance Describe ConferenceSchedule where
  type Property ConferenceSchedule = ScheduleProperty
  describeType = $(deriveDescribeType ''ConferenceSchedule)
  describeProperties _ ScheduleHasAtLeastOneSlot = Just "The schedule must contain at least one slot."
  describeProperties _ ScheduleSlotsAreContiguous = Just "Slots must be contiguous: each slot's end time must equal the next slot's start time."
  describeProperties _ ScheduleEachTalkOccursOnce = Just "Each talk must appear in exactly one slot (no duplicate talks)."
  propertyHolds s ScheduleHasAtLeastOneSlot = not (null (scheduleSlots s))
  propertyHolds s ScheduleSlotsAreContiguous =
    let slots = scheduleSlots s
        pairs = zip slots (drop 1 slots)
     in all (\(a, b) -> slotEnd a == slotStart b) pairs
  propertyHolds s ScheduleEachTalkOccursOnce =
    let titles = fmap (talkTitle . slotTalk) (scheduleSlots s)
     in length titles == length (nub titles)

-- * Prompt chain

{- | A multi-step prompt chain that builds a conference schedule from scratch.

Each step uses the output of earlier steps to constrain later prompts,
demonstrating prompt chaining with cross-step dependencies.

Steps:

1. 'Speakers' — generate 3-10 speakers
2. One 'Talk' per speaker (each speaker gives exactly one talk)
3. 'ConferenceSchedule' — the full day's schedule
-}
conferenceChain :: (Monad m) => PromptT m (Speakers, [Talk], ConferenceSchedule)
conferenceChain = do
  context "You are helping to schedule an academic computer science conference."
  context "The conference focuses on functional programming and type theory."

  allSpeakers <- prompt @Speakers

  context $
    "The confirmed speakers are: "
      <> mconcat (fmap (\s -> speakerName s <> " (" <> speakerAffiliation s <> "), ") (speakers allSpeakers))

  talks <-
    mapM
      ( \speaker -> do
          promptWith @Talk $
            "The talk must be presented by "
              <> speakerName speaker
              <> ". "
              <> "Invent a talk that fits their background."
      )
      (speakers allSpeakers)

  context $
    "The talks are: "
      <> mconcat (fmap (\t -> "\"" <> talkTitle t <> "\" by " <> talkSpeakerName t <> ", ") talks)

  schedule <-
    promptWith @ConferenceSchedule $
      "Create a Monday schedule as an ordered list of timetable slots, one per talk. "
        <> "Each slot must have a start time and end time (e.g. \"09:00\", \"09:45\"), and slots must be contiguous. "
        <> "Include every talk exactly once."

  pure (allSpeakers, talks, schedule)
