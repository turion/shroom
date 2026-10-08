{- | Re-exports 'Control.Monad.Prompt.Promptable.Promptable', the typeclass
that attaches a default prompting strategy to a type.

The program vocabulary itself — 'Control.Monad.Prompt.Effect.prompt',
'Control.Monad.Prompt.Effect.context', 'Control.Monad.Prompt.Effect.promptTools'
and the rest — lives in "Control.Monad.Prompt.Effect", expressed as
@effectful@ effects rather than as a bespoke monad transformer. Runner
configuration ('Control.Monad.Prompt.Effect.PromptConfig') and the
interpreters ('Control.Monad.Prompt.Effect.runPrompt',
'Control.Monad.Prompt.Effect.runPromptResultEff') live there too.

@
result <- 'Control.Monad.Prompt.Effect.runPromptResultEff' backend 'Control.Monad.Prompt.Effect.defaultPromptConfig' $ do
  'Control.Monad.Prompt.Effect.context' "The user's name is Alice."
  prompt \@User
@
-}
module Control.Monad.Prompt (module Control.Monad.Prompt.Promptable) where

-- shroom
import Control.Monad.Prompt.Promptable
