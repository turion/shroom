{- | "Control.Monad.Prompt" and "Control.Monad.Prompt.Effect" imported
unqualified, side by side, the way a user's program does (#27). The flat
request used to be called @prompt@ too, so @prompt@ was ambiguous here; now it
is the class method alone, and the flat request is 'promptSingle'.
-}
module UnqualifiedImports (tests) where

-- base
import Data.IORef (newIORef, readIORef)

-- text
import Data.Text (Text)
import Data.Text qualified as T

-- tasty
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

-- shroom
import Control.Monad.Prompt
import Control.Monad.Prompt.Effect

-- test
import TestUtils (MockConfig (..), mockBackend)
import Types (Counter (..))

tests :: TestTree
tests =
  testCase "prompt is unambiguous next to Effect's promptSingle and promptSingleWith" $ do
    seen <- newIORef ([] :: [Text])
    result <- runPromptResultEff (mockBackend (MockConfig seen)) defaultPromptConfig $ do
      context "unqualified"
      viaClass <- prompt @Counter
      flat <- promptSingle @Counter
      flatWith <- promptSingleWith @Counter "local only"
      pure (viaClass, flat, flatWith)
    case result of
      Left err -> assertFailure (T.unpack err)
      Right (Counter a, Counter b, Counter c) -> (a, b, c) @?= (0, 0, 0)
    calls <- readIORef seen
    length calls @?= 3
