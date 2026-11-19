{- |
Module      : Control.Monad.Prompt.FileMock
Description : A backend that replies from files, to test a prompt program without a model.

To test a shroom program, or to write a golden test for it, you want no network,
no API key and the same replies on every run. 'fileMockBackend' is a t'Backend'
that gives you that. It answers each request with the next numbered JSON file
(@response-001.json@, @response-002.json@, …) instead of calling a model. It also
prints the prompt of each request, so you see exactly what is sent for each step,
and you can edit the replies to try other scenarios.

@
cfg <- defaultFileMockConfig
result <- runPromptResultEff (fileMockBackend cfg) defaultPromptConfig (prompt \@User)
@

Here @User@ is any t'Control.Monad.Prompt.Promptable.Promptable' type, for example the one in "Control.Monad.Prompt".
Each file holds the JSON that a model would answer with for that step.
-}
module Control.Monad.Prompt.FileMock (FileMockConfig (..), defaultFileMockConfig, fileMockBackend) where

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
import Control.Monad.Prompt.Backend (Backend (..), BackendError (..), BackendReply (..), renderContextItems)
import Paths_shroom (getDataDir)

-- | Configuration for the file-based mock backend.
data FileMockConfig = FileMockConfig
  { responseDir :: FilePath
  {- ^ Directory holding the numbered response files. The default is
    @dev\/mock-responses\/@ in the data directory of @shroom@ (@getDataDir@ of the
    Cabal-generated @Paths_shroom@), so it does not depend on the current working
    directory. Set it to read your own files.
  -}
  , promptLogFn :: Text -> IO ()
  {- ^ Called before each reply with the full prompt text (context followed by the
    type description). The default prints it to stdout.
  -}
  , stepCounter :: IORef Int
  {- ^ Counts the replies given so far, so that the next request reads the next file.
    'defaultFileMockConfig' creates it at zero.
  -}
  }

{- | A t'FileMockConfig' that reads the files in @dev\/mock-responses\/@ of the @shroom@ data
directory and prints prompts to stdout.
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

{- | The t'Backend' for a t'FileMockConfig'. It ignores the offered tools, so every reply
is a 'BackendAnswer', and it answers a missing response file with
'BackendTransportError'.
-}
fileMockBackend :: (MonadIO m) => FileMockConfig -> Backend m
fileMockBackend cfg =
  Backend $ \ctx typeDesc _schema _toolDefs -> liftIO $ do
    n <- atomicModifyIORef' cfg.stepCounter (\i -> (i + 1, i))
    let stepNum = n + 1 -- 1-indexed for humans
        filename = "response-" <> printf "%03d" stepNum <> ".json"
        path = cfg.responseDir </> filename
        header = "=== PROMPT (step " <> T.pack (show stepNum) <> ") ==="
        footer = T.replicate (T.length header) "="
    cfg.promptLogFn $ T.unlines [header, renderContextItems ctx <> "\n" <> typeDesc, footer]
    exists <- doesFileExist path
    if not exists
      then pure $ Left $ BackendTransportError $ "FileMock: missing response file: " <> T.pack path
      else do
        contents <- TIO.readFile path
        pure $ Right (BackendAnswer (T.strip contents))
