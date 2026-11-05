{- | The 'Describable' and 'Surveyable' typeclasses: attach a human-readable
description, properties, and example values to a type, so that an
LLM backend — or anything else that renders a type for a reader, human or
model — can understand what it should produce.

== Typeclass hierarchy

@
Describable          — 'describeType': one sentence from Haddock (auto-derived)
    └── Surveyable   — 'Property', 'propertyHolds', 'describeProperties', 'examples'
                       ('Property' has no default: every instance states it, see below)
@

@Promptable@, which builds on 'Surveyable' to render a full prompt, lives on
the far side of the pure\/effectful boundary — in the @shroom@ package, as
@Control.Monad.Prompt.Promptable@ — see below.

Typical usage. Every 'Surveyable' instance states its @type Property@; there is
no default. The common choice is 'Structural': the properties of every field's
own type become properties of the whole value. It needs the @generics-sop@
classes @SOP.Generic@ and @SOP.HasDatatypeInfo@ (with @import qualified
Generics.SOP as SOP@, and @generics-sop@ among the package's @build-depends@):

@
-- | A user with a name and an email address.
data User = User { userName :: Text, userEmail :: Text }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, SOP.Generic, SOP.HasDatatypeInfo)

\$(deriveDescribable ''User)   -- generates instance Describable User from Haddock

instance Surveyable User where  -- the properties of 'Text' fields: there are none
  type Property User = Structural User

instance Promptable User  -- from Control.Monad.Prompt.Promptable; uses the
                          -- default description-based prompt rendering
@

A type that wants no properties at all, and no @generics-sop@ instances, says so in
one line, with no methods:

@
instance Surveyable User where
  type Property User = Void
@

A type that has properties of its own states them instead (and not those of its
fields), and then needs both 'propertyHolds' and 'describeProperties':

@
instance Surveyable User where
  type Property User = UserProperty
  propertyHolds u UserEmailNotEmpty = not (T.null (userEmail u))
  describeProperties _ UserEmailNotEmpty = Just "The email must not be empty."
@

To keep the field properties alongside its own, combine the two with 'Either'
(all four equations are needed). Writing this by hand is interim: a derived
\"Combined\" property source is planned in
<https://github.com/turion/shroom/issues/18 issue #18>:

@
instance Surveyable User where
  type Property User = Either UserProperty (Structural User)
  propertyHolds u (Left p) = … -- the type's own properties
  propertyHolds u (Right p) = structuralHolds u p
  describeProperties pu (Left p) = …
  describeProperties pu (Right p) = describeStructural pu p
@

An instance that leaves @type Property@ out does not compile. The 'Surveyable'
Haddock lists the common choices with their caveats (also an enumeration of your
own, forwarding a field's properties and a 'Foldable' container through 'Every'),
and quotes the errors a missing one gives.

== Pure\/effectful boundary

This module is pure, and the package boundary coincides with that line: it
lives in @shroom-class@, which has no program layer, while everything effectful
lives in @shroom@. It depends only on @aeson@, @text@, @containers@,
@openapi3@, @universe-base@, @generics-sop@ and @time@, and nothing under @Data.Shroom.@ may import
@Control.Monad.Prompt@ or any of its submodules. That is deliberate, not
incidental — a later arc (shroom-shikumi, "C3") needs exactly this pure half,
with no program layer attached, to feed shikumi's own instruction and
validation machinery. Keep it that way: an import of @Control.Monad.Prompt@
here would put the program layer back in C3's way.
-}
module Data.Shroom.Class (
  -- * Describable
  module Data.Shroom.Class.Describable,

  -- * Surveyable
  module Data.Shroom.Class.Surveyable,

  -- * Property checks
  PropertyCheck (..),
  propertyLines,
) where

import Data.Shroom.Class.Describable
import Data.Shroom.Class.PropertyCheck (PropertyCheck (..), propertyLines)
import Data.Shroom.Class.Surveyable
