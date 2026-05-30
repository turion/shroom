{- | Live conference scheduling demo against the real Claude API.

Runs the conference chain against Claude, printing each prompt and response
as they arrive.

Usage:

  ANTHROPIC_API_KEY=sk-ant-... cabal run shroom-claude
-}
module Main (main) where

-- base
import System.Environment (lookupEnv)

-- text
import Data.Text (pack)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

-- time
import Data.Time (UTCTime, defaultTimeLocale, formatTime)

-- sop-core
import Data.SOP (NP (..))

-- shroom
import Control.Monad.Prompt (PromptConfig (..), defaultPromptConfig, runPromptResultTWith, runPromptT)
import Control.Monad.Prompt.Anthropic (mkAnthropicConfig)
import Control.Monad.Prompt.Tool.Web (duckDuckGoSearchHandler, wikipediaSearchHandler)

-- conference scenario
import ConferenceTypes (
  ConferenceSchedule (..),
  Slot (..),
  Speaker (..),
  SpeakerName (..),
  Speakers (..),
  Talk (..),
  conferenceChainWithTools,
 )

fmtTime :: UTCTime -> String
fmtTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

main :: IO ()
main = do
  mKey <- lookupEnv "ANTHROPIC_API_KEY"
  case mKey of
    Nothing -> putStrLn "ANTHROPIC_API_KEY not set — exiting."
    Just key -> run (pack key)

run :: T.Text -> IO ()
run apiKey = do
  let cfg = mkAnthropicConfig apiKey
      pcfg = defaultPromptConfig {debugLog = Just TIO.putStrLn, maxToolSteps = Just 2}
      handlers = duckDuckGoSearchHandler :* wikipediaSearchHandler :* Nil
  result <- runPromptResultTWith cfg $ runPromptT pcfg handlers conferenceChainWithTools
  putStrLn ""
  case result of
    Left err ->
      putStrLn $ "ERROR: " <> T.unpack err
    Right (allSpeakers, talks, schedule) -> do
      putStrLn "\n=== RESULTS ==="
      putStrLn $ "Speakers: " <> show (length (speakers allSpeakers))
      mapM_
        ( \s ->
            let n = speakerName s
             in putStrLn $
                  "  - "
                    <> T.unpack (firstName n)
                    <> " "
                    <> T.unpack (lastName n)
                    <> " ("
                    <> T.unpack (speakerAffiliation s)
                    <> ")\nBio: "
                    <> T.unpack (speakerBio s)
        )
        (speakers allSpeakers)
      putStrLn $ "Talks:    " <> show (length talks)
      mapM_
        ( \t ->
            let n = talkSpeakerName t
             in putStrLn $
                  "  - \""
                    <> T.unpack (talkTitle t)
                    <> "\" by "
                    <> T.unpack (firstName n)
                    <> " "
                    <> T.unpack (lastName n)
        )
        talks
      putStrLn $ "Day:      " <> fmtTime (scheduleDay schedule)
      putStrLn $ "Slots:    " <> show (length (scheduleSlots schedule))
      mapM_
        ( \slot ->
            putStrLn $
              "  "
                <> fmtTime (slotStart slot)
                <> "-"
                <> fmtTime (slotEnd slot)
                <> ": \""
                <> T.unpack (talkTitle (slotTalk slot))
                <> "\""
        )
        (scheduleSlots schedule)
