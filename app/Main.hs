{- | Development executable for inspecting generated prompts.

Runs the conference scheduling prompt chain against the file-based mock
backend, printing each prompt to stdout before reading the response from
@dev\/mock-responses\/response-NNN.json@.

To use:

1. Run @cabal run shroom-dev@ to see the prompts and results.
2. Edit @dev\/mock-responses\/response-NNN.json@ to change what the mock LLM returns.
3. Iterate on prompt quality in @Data.Describe@ or @Control.Monad.Prompt@.
-}
module Main (main) where

-- text
import Data.Text qualified as T

-- shroom
import Control.Monad.Prompt (defaultPromptConfig, runPromptResultTWith, runPromptT)
import Control.Monad.Prompt.FileMock (defaultFileMockConfig)

-- conference scenario
import ConferenceTypes (ConferenceSchedule (..), Slot (..), Speaker (..), Speakers (..), Talk (..), conferenceChain)

main :: IO ()
main = do
  cfg <- defaultFileMockConfig
  result <- runPromptResultTWith cfg $ runPromptT defaultPromptConfig conferenceChain
  putStrLn ""
  case result of
    Left err ->
      putStrLn $ "ERROR: " <> T.unpack err
    Right (allSpeakers, talks, schedule) -> do
      putStrLn "=== RESULTS ==="
      putStrLn $ "Speakers: " <> show (length (speakers allSpeakers))
      mapM_
        ( \s ->
            putStrLn $
              "  - "
                <> T.unpack (speakerName s)
                <> " ("
                <> T.unpack (speakerAffiliation s)
                <> ")"
        )
        (speakers allSpeakers)
      putStrLn $ "Talks:    " <> show (length talks)
      mapM_
        ( \t ->
            putStrLn $
              "  - \""
                <> T.unpack (talkTitle t)
                <> "\" by "
                <> T.unpack (talkSpeakerName t)
        )
        talks
      putStrLn $ "Day:      " <> T.unpack (scheduleDay schedule)
      putStrLn $ "Slots:    " <> show (length (scheduleSlots schedule))
      mapM_
        ( \slot ->
            putStrLn $
              "  "
                <> T.unpack (slotStart slot)
                <> "-"
                <> T.unpack (slotEnd slot)
                <> ": \""
                <> T.unpack (talkTitle (slotTalk slot))
                <> "\""
        )
        (scheduleSlots schedule)
