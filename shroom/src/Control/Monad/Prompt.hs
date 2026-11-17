{- |
Module      : Control.Monad.Prompt
Description : Ask a language model for a typed Haskell value.

'prompt' asks a language model for a value of any 'Promptable' type. The model is
told the type's description, properties and examples (see "Data.Shroom.Class"), and
its answer is parsed and checked against them, with a retry when it fails.

Programs that chain requests, add context or offer tools are written in
"Control.Monad.Prompt.Effect", which also runs them against a backend (see
"Control.Monad.Prompt.Backend"). A backend for a real model comes from the
@shroom-baikai@ package; "Control.Monad.Prompt.FileMock" is one that needs no key and
no network.

The example requests one @User@ from Claude. It needs the packages @shroom@,
@shroom-baikai@, @aeson@, @generics-sop@, @openapi3@ and @text@, an @ANTHROPIC_API_KEY@
in the environment, and the GHC flag @-haddock@ for the module that declares @User@:
the description is read from the Haddock comment (see "Data.Shroom.Class.TH").

@
{\-# LANGUAGE DeriveAnyClass #-\}
{\-# LANGUAGE DerivingStrategies #-\}
{\-# LANGUAGE OverloadedStrings #-\}
{\-# LANGUAGE TemplateHaskell #-\}
{\-# LANGUAGE TypeApplications #-\}

module Main (main) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import System.Environment (getEnv)

import Control.Monad.Prompt
import Control.Monad.Prompt.Baikai (claudeBackend)
import Control.Monad.Prompt.Effect (context, defaultPromptConfig, runPromptResultEff)
import Data.Shroom.Class
import Data.Shroom.Class.TH (deriveDescribable)

-- | A user with a name and an email address.
data User = User
  { userName :: Text
  -- ^ The user's full name.
  , userEmail :: Text
  -- ^ The user's email address.
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, SOP.Generic, SOP.HasDatatypeInfo)

\$(deriveDescribable ''User)

instance Surveyable User

instance Promptable User

main :: IO ()
main = do
  apiKey <- T.pack \<$\> getEnv \"ANTHROPIC_API_KEY\"
  backend <- claudeBackend apiKey
  result <- runPromptResultEff backend defaultPromptConfig $ do
    context "Alice Smith signed up with alice\@example.org."
    prompt \@User
  either T.putStrLn print result
@
-}
module Control.Monad.Prompt (module Control.Monad.Prompt.Promptable, module Control.Monad.Prompt.Generic) where

-- shroom

-- Imported for its instances, which make @deriving via Generically X instance Promptable X@ work.
import Control.Monad.Prompt.Generic
import Control.Monad.Prompt.Promptable
