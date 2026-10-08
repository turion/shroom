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
  SpeakerName (..),
  Speaker (..),
  SpeakerProperty (..),
  Speakers (..),
  SpeakersProperty (..),
  GhcExtension (..),
  Talk (..),
  TalkProperty (..),
  Slot (..),
  SlotProperty (..),
  ConferenceSchedule (..),
  ScheduleProperty (..),
  conferenceChain,
  SpeakerLookup (..),
  fakeSpeakerLookupHandler,
  conferenceChainWithTools,
) where

-- base
import Data.List (nub)
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)

-- text
import Data.Text (Text)
import Data.Text qualified as T

-- time
import Data.Time (UTCTime, secondsToDiffTime, utctDayTime)

-- aeson
import Data.Aeson (FromJSON, ToJSON)

-- openapi3
import Data.OpenApi (ToSchema)

-- universe-base
import Data.Universe.Class (Universe)

-- effectful
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error)
import Effectful.State.Static.Local (State)

-- shroom
import Control.Monad.Prompt
import Control.Monad.Prompt.Backend (ContextItem)
import Control.Monad.Prompt.Effect (Prompt, context, orElse, promptWith, withContext)
import Control.Monad.Prompt.Effect qualified as Effect
import Control.Monad.Prompt.TH (deriveDescribable)
import Control.Monad.Prompt.Tool (ToolBinding, ToolHandler (..), Toolable (..))
import Data.Shroom.Class

-- * Types

-- | The full name of a person.
data SpeakerName = SpeakerName
  { firstName :: Text
  -- ^ The person's given name, e.g. "Simon".
  , lastName :: Text
  -- ^ The person's family name, e.g. "Peyton Jones".
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, ToSchema)

-- | A speaker at ZuriHac, identified by name, affiliation, and a short bio.
data Speaker = Speaker
  { speakerName :: SpeakerName
  -- ^ The speaker's name.
  , speakerAffiliation :: Text
  -- ^ The speaker's institutional affiliation, e.g. "University of Cambridge".
  , speakerBio :: Text
  -- ^ A one-paragraph biography of the speaker.
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

-- | A property of a speaker that should hold.
data SpeakerProperty
  = SpeakerFirstNameNotEmpty
  | SpeakerLastNameNotEmpty
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

{- | A GHC language extension that may be required to understand or appreciate a talk.
If your talk requires all of these, please also bring a whiteboard.
-}
data GhcExtension
  = -- | For when you want your types to have a family reunion nobody asked for.
    TypeFamilies
  | -- | The extension that says "I trust you" while slowly backing away.
    UndecidableInstances
  | -- | Finally, the extension for types that can't make up their mind.
    ImpredicativeTypes
  | -- | Use each value exactly once. No, you can't have it back.
    LinearTypes
  deriving (Bounded, Enum, Universe, Eq, Ord, Show, Generic, ToJSON, FromJSON, ToSchema)

-- | A talk submitted to a conference session, with title, abstract, speaker, and required GHC extensions.
data Talk = Talk
  { talkTitle :: Text
  -- ^ The title of the talk.
  , talkAbstract :: Text
  -- ^ A brief abstract describing the talk's content (1-3 sentences).
  , talkSpeakerName :: SpeakerName
  -- ^ The name of the presenting speaker. Must exactly match a known speaker's name.
  , talkExtensions :: [GhcExtension]
  -- ^ GHC language extensions that attendees should know to fully appreciate this talk.
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

-- | A property of a talk that should hold.
data TalkProperty
  = TalkTitleNotEmpty
  | TalkSpeakerNameNotEmpty
  deriving (Bounded, Enum, Universe, Eq, Ord, Show)

-- | A timetable slot at the conference, containing a single talk with its start and end time.
data Slot = Slot
  { slotStart :: UTCTime
  -- ^ The start time of the slot (ISO 8601), e.g. "2027-06-11T09:00:00Z".
  , slotEnd :: UTCTime
  -- ^ The end time of the slot (ISO 8601), e.g. "2027-06-11T09:45:00Z".
  , slotTalk :: Talk
  -- ^ The talk presented in this slot.
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

-- | A property of a slot that should hold.
data SlotProperty
  = SlotStartBeforeEnd
  deriving (Bounded, Enum, Universe, Eq, Ord, Show)

-- | A full day's schedule for the conference, listing all timetable slots in order.
data ConferenceSchedule = ConferenceSchedule
  { scheduleDay :: UTCTime
  -- ^ The date of this conference day (ISO 8601), e.g. "2027-06-11T00:00:00Z".
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
  | ScheduleStartsAtNine
  deriving (Bounded, Enum, Universe, Eq, Ord, Show)

$(deriveDescribable ''SpeakerName)

instance Surveyable SpeakerName

instance Promptable SpeakerName

$(deriveDescribable ''Speaker)

instance Surveyable Speaker where
  type Property Speaker = SpeakerProperty
  describeProperties _ SpeakerFirstNameNotEmpty = Just "The speaker's first name must not be empty."
  describeProperties _ SpeakerLastNameNotEmpty = Just "The speaker's last name must not be empty."
  describeProperties _ SpeakerAffiliationNotEmpty = Just "The speaker's affiliation must not be empty."
  propertyHolds s SpeakerLastNameNotEmpty = not (T.null (lastName (speakerName s)))
  propertyHolds s SpeakerFirstNameNotEmpty = not (T.null (firstName (speakerName s)))
  propertyHolds s SpeakerAffiliationNotEmpty = not (T.null (speakerAffiliation s))

instance Promptable Speaker

$(deriveDescribable ''Speakers)

instance Surveyable Speakers where
  type Property Speakers = Either SpeakersProperty SpeakerProperty
  describeProperties _ (Left SpeakersBetween3And10) = Just "The list must contain between 3 and 10 speakers (inclusive)."
  describeProperties _ (Right p) = describeEvery "Every speaker" (Proxy @Speaker) p
  propertyHolds s (Left SpeakersBetween3And10) = let n = length (speakers s) in n >= 3 && n <= 10
  propertyHolds s (Right p) = everyHolds (speakers s) p

instance Promptable Speakers

$(deriveDescribable ''Talk)

instance Surveyable Talk where
  type Property Talk = TalkProperty
  describeProperties _ TalkTitleNotEmpty = Just "The talk title must not be empty."
  describeProperties _ TalkSpeakerNameNotEmpty = Just "The speaker's first and last name must not be empty."
  propertyHolds t TalkTitleNotEmpty = not (T.null (talkTitle t))
  propertyHolds t TalkSpeakerNameNotEmpty = not (T.null (firstName (talkSpeakerName t))) && not (T.null (lastName (talkSpeakerName t)))

instance Promptable Talk

$(deriveDescribable ''Slot)

instance Surveyable Slot where
  type Property Slot = SlotProperty
  describeProperties _ SlotStartBeforeEnd = Just "The slot's start time must be strictly before its end time."
  propertyHolds s SlotStartBeforeEnd = slotStart s < slotEnd s

instance Promptable Slot

$(deriveDescribable ''ConferenceSchedule)

instance Surveyable ConferenceSchedule where
  type Property ConferenceSchedule = Either ScheduleProperty SlotProperty
  describeProperties _ (Left ScheduleHasAtLeastOneSlot) = Just "The schedule must contain at least one slot."
  describeProperties _ (Left ScheduleSlotsAreContiguous) = Just "Slots must be contiguous: each slot's end time must equal the next slot's start time."
  describeProperties _ (Left ScheduleEachTalkOccursOnce) = Just "Each talk must appear in exactly one slot (no duplicate talks)."
  describeProperties _ (Left ScheduleStartsAtNine) = Just "ZuriHac always starts at 09:00 local time (07:00 UTC in June). The first slot must start then."
  describeProperties _ (Right p) = describeEvery "Every slot" (Proxy @Slot) p
  propertyHolds s (Left ScheduleHasAtLeastOneSlot) = not (null (scheduleSlots s))
  propertyHolds s (Left ScheduleSlotsAreContiguous) =
    let slots = scheduleSlots s
        pairs = zip slots (drop 1 slots)
     in all (\(a, b) -> slotEnd a == slotStart b) pairs
  propertyHolds s (Left ScheduleEachTalkOccursOnce) =
    let titles = fmap (talkTitle . slotTalk) (scheduleSlots s)
     in length titles == length (nub titles)
  propertyHolds s (Left ScheduleStartsAtNine) =
    case scheduleSlots s of
      [] -> False
      (first : _) -> utctDayTime (slotStart first) == secondsToDiffTime (7 * 3600)
  propertyHolds s (Right p) = everyHolds (scheduleSlots s) p

instance Promptable ConferenceSchedule

-- * Prompt chain

{- | A multi-step prompt chain that builds a ZuriHac 2027 conference schedule from scratch.

Each step uses the output of earlier steps to constrain later prompts,
demonstrating prompt chaining with cross-step dependencies.

Steps:

1. 'Speakers' — generate 3-10 speakers
2. One 'Talk' per speaker (each speaker gives exactly one talk)
3. 'ConferenceSchedule' — the full day's schedule
-}
conferenceChain ::
  (Prompt :> es, State [ContextItem] :> es, Error Text :> es) =>
  Eff es (Speakers, [Talk], ConferenceSchedule)
conferenceChain = do
  context "You are helping to schedule ZuriHac 2027, a Haskell community conference at OST Rapperswil-Jona, Switzerland, right next to a beautiful lake."
  context "The conference focuses on Haskell, functional programming, and type theory -- or as we call it, 'a weekend of explaining monads to lake ducks'."

  allSpeakers <- prompt @Speakers

  context $
    "The confirmed speakers are: "
      <> T.intercalate
        ", "
        ( fmap
            ( \s ->
                firstName (speakerName s)
                  <> " "
                  <> lastName (speakerName s)
                  <> " ("
                  <> speakerAffiliation s
                  <> ")"
            )
            (speakers allSpeakers)
        )

  talks <-
    mapM
      ( \speaker -> do
          promptWith @Talk $
            "The talk must be presented by "
              <> firstName (speakerName speaker)
              <> " "
              <> lastName (speakerName speaker)
              <> ". "
              <> "Invent a talk that fits their background."
      )
      (speakers allSpeakers)

  context $
    "The talks are: "
      <> T.intercalate
        ", "
        ( fmap
            ( \t ->
                "\""
                  <> talkTitle t
                  <> "\" by "
                  <> firstName (talkSpeakerName t)
                  <> " "
                  <> lastName (talkSpeakerName t)
            )
            talks
        )

  -- Try a short prompt first (cheap); if the model can't satisfy the type properties
  -- after retries, fall back to the detailed prompt that explicitly spells them out.
  schedule <-
    promptWith @ConferenceSchedule
      "Create a schedule for Friday 11 June 2027 with one slot per talk. Include every talk."
      `orElse` promptWith @ConferenceSchedule
        ( "Create a schedule for Friday 11 June 2027 as an ordered list of timetable slots, one per talk. "
            <> "Each slot must have a start time and end time in ISO 8601 UTC format (e.g. \"2027-06-11T07:00:00Z\", \"2027-06-11T07:45:00Z\"), and slots must be contiguous. "
            <> "The first slot must start at 07:00:00 UTC (09:00 Zurich local time). "
            <> "Include every talk exactly once."
        )

  pure (allSpeakers, talks, schedule)

-- * Tool: SpeakerLookup

-- | A request to look up background info about a conference speaker by name.
newtype SpeakerLookup = SpeakerLookup
  { speakerLookupName :: Text
  -- ^ The full name of the speaker to look up.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(deriveDescribable ''SpeakerLookup)

instance Surveyable SpeakerLookup

instance Promptable SpeakerLookup

instance Toolable SpeakerLookup where
  toolDescription _ = Just "Look up a speaker's background, research interests, and past talks."

-- | Canned handler: returns one of 3 bios based on name length mod 3.
fakeSpeakerLookupHandler :: ToolHandler SpeakerLookup
fakeSpeakerLookupHandler = ToolHandler $ \(SpeakerLookup name) ->
  pure $ Right $ case T.length name `mod` 3 of
    0 -> name <> " is known for pioneering work on dependent types and proof assistants."
    1 -> name <> " is a compiler engineer specializing in GHC optimizations and LLVM backends."
    _ -> name <> " researches distributed systems and formal verification of consensus protocols."

{- | Conference chain that instructs the LLM to search the web for speaker
info. Unlike 'conferenceChain', this takes the tool @bindings@ to offer —
only the per-speaker 'Talk' step gets them, matching the instruction text
below; build each binding with 'Control.Monad.Prompt.Tool.toolBinding' and
interpret with 'Control.Monad.Prompt.Tool.runTool'.
-}
conferenceChainWithTools ::
  (Prompt :> es, State [ContextItem] :> es, Error Text :> es) =>
  [ToolBinding (Eff es)] ->
  Eff es (Speakers, [Talk], ConferenceSchedule)
conferenceChainWithTools bindings = do
  context "You are helping to schedule ZuriHac 2027, a Haskell community conference at OST Rapperswil-Jona, Switzerland, right next to a beautiful lake."
  context "The conference focuses on Haskell, functional programming, and type theory."
  allSpeakers <- prompt @Speakers
  context $
    "The confirmed speakers are: "
      <> T.intercalate
        ", "
        ( fmap
            ( \s ->
                firstName (speakerName s)
                  <> " "
                  <> lastName (speakerName s)
            )
            (speakers allSpeakers)
        )
  talks <-
    mapM
      ( \speaker ->
          withContext
            ( "The talk must be presented by "
                <> firstName (speakerName speaker)
                <> " "
                <> lastName (speakerName speaker)
                <> ". You may search the web once to look up their background. Then write the abstract — do not search again."
            )
            (Effect.promptTools @Talk bindings)
      )
      (speakers allSpeakers)
  context $
    "The talks are: "
      <> T.intercalate ", " (fmap talkTitle talks)
  schedule <-
    promptWith @ConferenceSchedule
      "Create a schedule for Friday 11 June 2027 with one slot per talk. Include every talk."
      `orElse` promptWith @ConferenceSchedule
        "Create a schedule for Friday 11 June 2027. First slot starts at 07:00:00 UTC. Slots must be contiguous."
  pure (allSpeakers, talks, schedule)
