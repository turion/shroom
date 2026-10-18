{- | Re-exports 'Control.Monad.Prompt.Promptable.Promptable', the typeclass
that attaches a default prompting strategy to a type.

The program vocabulary itself — 'Control.Monad.Prompt.Effect.promptSingle',
'Control.Monad.Prompt.Effect.context', 'Control.Monad.Prompt.Effect.promptTools'
and the rest — lives in "Control.Monad.Prompt.Effect", expressed as
@effectful@ effects rather than as a bespoke monad transformer. Runner
configuration ('Control.Monad.Prompt.Effect.PromptConfig') and the
interpreters ('Control.Monad.Prompt.Effect.runPrompt',
'Control.Monad.Prompt.Effect.runPromptResultEff') live there too.

The class method 'Control.Monad.Prompt.Promptable.prompt' comes from this module, the
runner and 'Control.Monad.Prompt.Effect.context' from "Control.Monad.Prompt.Effect",
so a program imports both (the names do not clash, since "Control.Monad.Prompt.Effect"
exports no @prompt@):

@
import Control.Monad.Prompt
import Control.Monad.Prompt.Effect (context, defaultPromptConfig, runPromptResultEff)

result <- runPromptResultEff backend defaultPromptConfig $ do
  context "The user's name is Alice."
  prompt \@User
@
-}
module Control.Monad.Prompt (module Control.Monad.Prompt.Promptable, module Control.Monad.Prompt.Generic) where

-- shroom

-- Imported for its instances, which make @deriving via Generically X instance Promptable X@ work.
import Control.Monad.Prompt.Generic
import Control.Monad.Prompt.Promptable
