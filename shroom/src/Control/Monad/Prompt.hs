{- |
Module      : Control.Monad.Prompt
Description : Ask a language model for a typed Haskell value.

'prompt' asks a language model for a value of any 'Promptable' type. The model is
told the type's description, properties and examples (see "Data.Shroom.Class"), and
its answer is parsed and checked against them, with a retry when it fails.

Programs that chain requests, add context or offer tools are written in
"Control.Monad.Prompt.Effect". The program below asks for one @User@. Its
description is read from the Haddock comments, so the module that declares @User@
needs the GHC flag @-haddock@ (see "Data.Shroom.Class.TH").

@
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

signUp :: (Prompt :> es, State [ContextItem] :> es) => Eff es User
signUp = do
  context \"Alice Smith signed up with alice\@example.org.\"
  prompt \@User
@

The program is not tied to a model. An interpreter runs it against a backend (see
"Control.Monad.Prompt.Backend"), as 'Control.Monad.Prompt.Effect.runPromptResultEff'
does. "Control.Monad.Prompt.FileMock" is a backend that needs no key and no network.
For a real model, the @shroom-baikai@ package builds a backend, for example
<https://hackage.haskell.org/package/shroom-baikai/docs/Control-Monad-Prompt-Baikai.html#v:claudeBackend claudeBackend>
for Claude.
-}
module Control.Monad.Prompt (module Control.Monad.Prompt.Promptable, module Control.Monad.Prompt.Generic) where

-- shroom

-- Imported for its instances, which make @deriving via Generically X instance Promptable X@ work.
import Control.Monad.Prompt.Generic
import Control.Monad.Prompt.Promptable
