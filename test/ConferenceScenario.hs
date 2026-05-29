module Main (main) where

-- base
import Data.IORef
import Data.List (nub)

-- text
import Data.Text (Text)
import Data.Text qualified as T

-- tasty
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- shroom
import Control.Monad.Prompt

-- test
import ConferenceTypes
import TestUtils

-- Canned responses matching the new chain:
--   1. Speakers (3 speakers)
--   2. Talk for speaker 1
--   3. Talk for speaker 2
--   4. Talk for speaker 3
--   5. ConferenceSchedule (3 contiguous slots, starting at 07:00 UTC)

speakersJson :: Text
speakersJson =
  "{\"speakers\":["
    <> "{\"speakerName\":{\"firstName\":\"Grace\",\"lastName\":\"Hopper\"},\"speakerAffiliation\":\"Yale University\",\"speakerBio\":\"Pioneer of compiler design.\"},"
    <> "{\"speakerName\":{\"firstName\":\"Alan\",\"lastName\":\"Turing\"},\"speakerAffiliation\":\"University of Manchester\",\"speakerBio\":\"Father of theoretical computer science.\"},"
    <> "{\"speakerName\":{\"firstName\":\"John\",\"lastName\":\"McCarthy\"},\"speakerAffiliation\":\"Stanford University\",\"speakerBio\":\"Creator of Lisp and coiner of AI.\"}"
    <> "]}"

talk1Json :: Text
talk1Json = "{\"talkTitle\":\"Compilers Are For Everyone\",\"talkAbstract\":\"A history of the compiler.\",\"talkSpeakerName\":{\"firstName\":\"Grace\",\"lastName\":\"Hopper\"},\"talkExtensions\":[\"TypeFamilies\"]}"

talk2Json :: Text
talk2Json = "{\"talkTitle\":\"Computability and the Halting Problem\",\"talkAbstract\":\"On undecidability and limits of computation.\",\"talkSpeakerName\":{\"firstName\":\"Alan\",\"lastName\":\"Turing\"},\"talkExtensions\":[\"UndecidableInstances\"]}"

talk3Json :: Text
talk3Json = "{\"talkTitle\":\"The Birth of Lisp\",\"talkAbstract\":\"How symbolic computation changed programming.\",\"talkSpeakerName\":{\"firstName\":\"John\",\"lastName\":\"McCarthy\"},\"talkExtensions\":[]}"

scheduleJson :: Text
scheduleJson =
  "{\"scheduleDay\":\"2027-06-11T00:00:00Z\",\"scheduleSlots\":["
    <> "{\"slotStart\":\"2027-06-11T07:00:00Z\",\"slotEnd\":\"2027-06-11T07:45:00Z\",\"slotTalk\":"
    <> talk1Json
    <> "},"
    <> "{\"slotStart\":\"2027-06-11T07:45:00Z\",\"slotEnd\":\"2027-06-11T08:30:00Z\",\"slotTalk\":"
    <> talk2Json
    <> "},"
    <> "{\"slotStart\":\"2027-06-11T08:30:00Z\",\"slotEnd\":\"2027-06-11T09:15:00Z\",\"slotTalk\":"
    <> talk3Json
    <> "}"
    <> "]}"

allResponses :: IO (IORef [Text])
allResponses = newIORef [speakersJson, talk1Json, talk2Json, talk3Json, scheduleJson]

main :: IO ()
main =
  defaultMain $
    testGroup
      "conference scenario"
      [ testCase "chain completes successfully" $ do
          responses <- allResponses
          seenCtxs <- newIORef ([] :: [Text])
          let cfg = SeqMockConfig responses seenCtxs
          result <- runPromptResultTWith cfg $ runPromptT defaultPromptConfig conferenceChain
          case result of
            Left err -> fail $ "Expected Right but got Left: " <> T.unpack err
            Right _ -> pure ()
      , testCase "3 speakers are returned" $ do
          responses <- allResponses
          seenCtxs <- newIORef ([] :: [Text])
          let cfg = SeqMockConfig responses seenCtxs
          result <- runPromptResultTWith cfg $ runPromptT defaultPromptConfig conferenceChain
          case result of
            Left err -> fail $ "Expected Right but got Left: " <> T.unpack err
            Right (allSpeakers, _, _) ->
              length (speakers allSpeakers) @?= 3
      , testCase "one talk per speaker, speaker names match" $ do
          responses <- allResponses
          seenCtxs <- newIORef ([] :: [Text])
          let cfg = SeqMockConfig responses seenCtxs
          result <- runPromptResultTWith cfg $ runPromptT defaultPromptConfig conferenceChain
          case result of
            Left err -> fail $ "Expected Right but got Left: " <> T.unpack err
            Right (allSpeakers, talks, _) -> do
              length talks @?= length (speakers allSpeakers)
              mapM_
                (\(speaker, talk) -> talkSpeakerName talk @?= speakerName speaker)
                (zip (speakers allSpeakers) talks)
      , testCase "schedule has at least one slot" $ do
          responses <- allResponses
          seenCtxs <- newIORef ([] :: [Text])
          let cfg = SeqMockConfig responses seenCtxs
          result <- runPromptResultTWith cfg $ runPromptT defaultPromptConfig conferenceChain
          case result of
            Left err -> fail $ "Expected Right but got Left: " <> T.unpack err
            Right (_, _, schedule) ->
              not (null (scheduleSlots schedule)) @?= True
      , testCase "schedule slots are contiguous" $ do
          responses <- allResponses
          seenCtxs <- newIORef ([] :: [Text])
          let cfg = SeqMockConfig responses seenCtxs
          result <- runPromptResultTWith cfg $ runPromptT defaultPromptConfig conferenceChain
          case result of
            Left err -> fail $ "Expected Right but got Left: " <> T.unpack err
            Right (_, _, schedule) -> do
              let slots = scheduleSlots schedule
                  pairs = zip slots (drop 1 slots)
              all (\(a, b) -> slotEnd a == slotStart b) pairs @?= True
      , testCase "schedule starts at 07:00 UTC (09:00 Zurich)" $ do
          responses <- allResponses
          seenCtxs <- newIORef ([] :: [Text])
          let cfg = SeqMockConfig responses seenCtxs
          result <- runPromptResultTWith cfg $ runPromptT defaultPromptConfig conferenceChain
          case result of
            Left err -> fail $ "Expected Right but got Left: " <> T.unpack err
            Right (_, _, schedule) ->
              case scheduleSlots schedule of
                [] -> fail "No slots"
                (first : _) -> slotStart first @?= read "2027-06-11 07:00:00 UTC"
      , testCase "step 2 context includes all speaker names" $ do
          responses <- allResponses
          seenCtxs <- newIORef ([] :: [Text])
          let cfg = SeqMockConfig responses seenCtxs
          _ <- runPromptResultTWith cfg $ runPromptT defaultPromptConfig conferenceChain
          seen <- readIORef seenCtxs
          -- Step 2 (index 1) should contain the speaker names injected into context
          assertContains "Hopper" (seen !! 1)
          assertContains "Turing" (seen !! 1)
          assertContains "McCarthy" (seen !! 1)
      , testCase "last context includes all talk titles" $ do
          responses <- allResponses
          seenCtxs <- newIORef ([] :: [Text])
          let cfg = SeqMockConfig responses seenCtxs
          _ <- runPromptResultTWith cfg $ runPromptT defaultPromptConfig conferenceChain
          seen <- readIORef seenCtxs
          -- Step 5 (index 4) should contain all talk titles injected into context
          assertContains "Compilers Are For Everyone" (seen !! 4)
          assertContains "Computability and the Halting" (seen !! 4)
          assertContains "The Birth of Lisp" (seen !! 4)
      , testCase "exactly 5 LLM calls are made (1 speakers + 3 talks + 1 schedule)" $ do
          responses <- allResponses
          seenCtxs <- newIORef ([] :: [Text])
          let cfg = SeqMockConfig responses seenCtxs
          _ <- runPromptResultTWith cfg $ runPromptT defaultPromptConfig conferenceChain
          seen <- readIORef seenCtxs
          length seen @?= 5
      , testCase "duplicate talk in schedule triggers retry" $ do
          -- First schedule has talk1 in two slots (fails ScheduleEachTalkOccursOnce)
          -- Second schedule is valid
          let dupScheduleJson =
                "{\"scheduleDay\":\"2027-06-11T00:00:00Z\",\"scheduleSlots\":["
                  <> "{\"slotStart\":\"2027-06-11T07:00:00Z\",\"slotEnd\":\"2027-06-11T07:45:00Z\",\"slotTalk\":"
                  <> talk1Json
                  <> "},"
                  <> "{\"slotStart\":\"2027-06-11T07:45:00Z\",\"slotEnd\":\"2027-06-11T08:30:00Z\",\"slotTalk\":"
                  <> talk1Json
                  <> "},"
                  <> "{\"slotStart\":\"2027-06-11T08:30:00Z\",\"slotEnd\":\"2027-06-11T09:15:00Z\",\"slotTalk\":"
                  <> talk3Json
                  <> "}"
                  <> "]}"
          responses <- newIORef [speakersJson, talk1Json, talk2Json, talk3Json, dupScheduleJson, scheduleJson]
          seenCtxs <- newIORef ([] :: [Text])
          let cfg = SeqMockConfig responses seenCtxs
          result <- runPromptResultTWith cfg $ runPromptT (defaultPromptConfig {maxRetries = 2}) conferenceChain
          case result of
            Left err -> fail $ "Expected Right but got Left: " <> T.unpack err
            Right (_, _, schedule) -> do
              let titles = fmap (talkTitle . slotTalk) (scheduleSlots schedule)
              length titles @?= length (nub titles)
          seen <- readIORef seenCtxs
          -- 5 normal calls + 1 retry for schedule = 6 total
          length seen @?= 6
          -- Retry context should mention the violated invariant
          assertContains "IMPORTANT" (seen !! 5)
          assertContains "duplicate" (seen !! 5)
      , testCase "non-contiguous slots in schedule triggers retry" $ do
          -- First schedule has a gap between slots (fails ScheduleSlotsAreContiguous)
          let gapScheduleJson =
                "{\"scheduleDay\":\"2027-06-11T00:00:00Z\",\"scheduleSlots\":["
                  <> "{\"slotStart\":\"2027-06-11T07:00:00Z\",\"slotEnd\":\"2027-06-11T07:45:00Z\",\"slotTalk\":"
                  <> talk1Json
                  <> "},"
                  <> "{\"slotStart\":\"2027-06-11T08:00:00Z\",\"slotEnd\":\"2027-06-11T08:45:00Z\",\"slotTalk\":"
                  <> talk2Json
                  <> "},"
                  <> "{\"slotStart\":\"2027-06-11T08:45:00Z\",\"slotEnd\":\"2027-06-11T09:30:00Z\",\"slotTalk\":"
                  <> talk3Json
                  <> "}"
                  <> "]}"
          responses <- newIORef [speakersJson, talk1Json, talk2Json, talk3Json, gapScheduleJson, scheduleJson]
          seenCtxs <- newIORef ([] :: [Text])
          let cfg = SeqMockConfig responses seenCtxs
          result <- runPromptResultTWith cfg $ runPromptT (defaultPromptConfig {maxRetries = 2}) conferenceChain
          case result of
            Left err -> fail $ "Expected Right but got Left: " <> T.unpack err
            Right (_, _, schedule) -> do
              let slots = scheduleSlots schedule
                  pairs = zip slots (drop 1 slots)
              all (\(a, b) -> slotEnd a == slotStart b) pairs @?= True
          seen <- readIORef seenCtxs
          length seen @?= 6
          assertContains "IMPORTANT" (seen !! 5)
          assertContains "contiguous" (seen !! 5)
      , testCase "speakers property validation: too few speakers triggers retry" $ do
          -- First response has only 2 speakers (fails SpeakersBetween3And10)
          -- Second response has 3 speakers (valid)
          let badSpeakers =
                "{\"speakers\":["
                  <> "{\"speakerName\":{\"firstName\":\"A\",\"lastName\":\"B\"},\"speakerAffiliation\":\"X\",\"speakerBio\":\".\"},"
                  <> "{\"speakerName\":{\"firstName\":\"C\",\"lastName\":\"D\"},\"speakerAffiliation\":\"Y\",\"speakerBio\":\".\"}"
                  <> "]}"
          responses <- newIORef [badSpeakers, speakersJson, talk1Json, talk2Json, talk3Json, scheduleJson]
          seenCtxs <- newIORef ([] :: [Text])
          let cfg = SeqMockConfig responses seenCtxs
          result <- runPromptResultTWith cfg $ runPromptT (defaultPromptConfig {maxRetries = 2}) conferenceChain
          case result of
            Left err -> fail $ "Expected Right but got Left: " <> T.unpack err
            Right (allSpeakers, _, _) ->
              length (speakers allSpeakers) @?= 3
          seen <- readIORef seenCtxs
          -- 1 bad speakers + 1 good speakers + 3 talks + 1 schedule = 6 calls
          length seen @?= 6
          -- Retry context should explain the violated invariant
          assertContains "IMPORTANT" (seen !! 1)
          assertContains "between 3 and 10" (seen !! 1)
      ]
