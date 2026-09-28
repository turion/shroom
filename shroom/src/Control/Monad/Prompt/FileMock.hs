{- | A file-based mock 'LLMBackend' for development and prompt inspection.

Instead of calling a real LLM, 'FileMockConfig' reads pre-written JSON
responses from numbered files on disk (@response-001.json@, etc.) and
prints the full prompt text to stdout (or any 'IO' action you provide)
before each call.

This lets you:

* See exactly what prompt text is sent to the LLM for each step
* Edit individual response files to test different scenarios
* Iterate on prompt quality without incurring API costs

Usage:

@
cfg <- defaultFileMockConfig
result <- runPromptResultTWith cfg $ runPromptT defaultPromptConfig myChain
@

Response files are read from the package's @dev\/mock-responses\/@ directory by
default, resolved against the package root rather than the process's current
working directory — so @cabal run shroom-dev@ finds them regardless of where
it is invoked from. Create @response-001.json@, @response-002.json@, etc. with
the JSON that a real LLM would return for each step.
-}
module Control.Monad.Prompt.FileMock (FileMockConfig (..), defaultFileMockConfig) where

-- base
import Control.Monad.IO.Class (MonadIO (..))
import Data.IORef
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Text.Printf (printf)

-- text
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

-- shroom
import Control.Monad.Prompt (LLMBackend (..), renderContextItems)
import Paths_shroom (getDataDir)

-- | Configuration for the file-based mock backend.
data FileMockConfig = FileMockConfig
  { responseDir :: FilePath
  {- ^ Directory containing numbered response files (@response-001.json@, …).
    Default: the package's @dev\/mock-responses\/@, resolved via the Cabal-generated
    'getDataDir' so it works regardless of the process's current working directory.
  -}
  , promptLogFn :: Text -> IO ()
  {- ^ Called with the full prompt text (context \<\> type description) before
    each LLM call.  Default: 'TIO.putStrLn'.
  -}
  , stepCounter :: IORef Int
  {- ^ Internal counter tracking which response file to read next (0-indexed
    internally, 1-indexed in filenames).  Created by 'defaultFileMockConfig'.
  -}
  }

{- | Create a 'FileMockConfig' with default settings:
reads from the package's @dev\/mock-responses\/@ directory, logs prompts to stdout.
-}
defaultFileMockConfig :: IO FileMockConfig
defaultFileMockConfig = do
  counter <- newIORef 0
  dataDir <- getDataDir
  pure
    FileMockConfig
      { responseDir = dataDir </> "dev/mock-responses"
      , promptLogFn = TIO.putStrLn
      , stepCounter = counter
      }

instance LLMBackend FileMockConfig where
  runChatWithTools cfg _promptCfg ctx typeDesc _schema _toolDefs _dispatch _maxToolSteps = liftIO $ do
    n <- atomicModifyIORef' cfg.stepCounter (\i -> (i + 1, i))
    let stepNum = n + 1 -- 1-indexed for humans
        filename = "response-" <> printf "%03d" stepNum <> ".json"
        path = cfg.responseDir </> filename
        header = "=== PROMPT (step " <> T.pack (show stepNum) <> ") ==="
        footer = T.replicate (T.length header) "="
    cfg.promptLogFn $ T.unlines [header, renderContextItems ctx <> "\n" <> typeDesc, footer]
    exists <- doesFileExist path
    if not exists
      then pure $ Left $ "FileMock: missing response file: " <> T.pack path
      else do
        contents <- TIO.readFile path
        pure $ Right (T.strip contents)
