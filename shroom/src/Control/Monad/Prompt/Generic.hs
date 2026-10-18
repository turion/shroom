{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Prompt a type part by part, after choosing its constructor if it has
several: @deriving via 'Generically'@. This module is the user's guide.

== Opting in

Three lines of setup, next to the instances you write anyway. In your @.cabal@
file, @generics-sop@ among the @build-depends@ (next to @shroom@):

@
build-depends: shroom, generics-sop
@

and in the module (it needs @DerivingVia@, @StandaloneDeriving@, @DeriveAnyClass@,
@DeriveGeneric@ and, for 'Control.Monad.Prompt.TH.deriveDescribable',
@TemplateHaskell@):

@
import Generics.SOP qualified as SOP
@

Then a record, with a flat leaf type for each of its parts. The leaf type is
derived first: a field type needs its 'Describable' instance from an earlier
splice (or the same one), so that its description reaches the model with the
record's:

@
-- | The full name of a person.
newtype Name = Name Text
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, SOP.Generic, SOP.HasDatatypeInfo)

\$(deriveDescribable ''Name)

instance Surveyable Name   -- or a custom Property, e.g. \"not empty\"

instance Promptable Name   -- flat: one request

-- | A musician.
data Musician = Musician
  { name :: Name
  -- ^ The musician's full name.
  , city :: City
  -- ^ The city the musician is from.
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, SOP.Generic, SOP.HasDatatypeInfo)

\$(deriveDescribable ''Musician)

instance Surveyable Musician

deriving via Generically Musician instance Promptable Musician
@

A sum is derived the same way: @data Fruit = FruitApple Apple | FruitBanana
Banana@ with @deriving via Generically Fruit instance Promptable Fruit@ first
asks for the constructor, then builds that constructor's fields.

The type needs the same instances as for a flat 'Promptable' ('Surveyable',
'ToSchema', 'FromJSON'), besides them the @generics-sop@ classes @SOP.Generic@
and @SOP.HasDatatypeInfo@ (derivable with @deriving anyclass@), and a
'Promptable' instance for every field type.

== Only the class method takes the strategy

@deriving via Generically@ takes effect only through the class method
'Control.Monad.Prompt.Promptable.prompt' (and so through any 'Promptable'
instance that calls it, such as a field's). The flat
'Control.Monad.Prompt.Effect.prompt' stays flat: it sends one request for the
whole value, never calls the type's own @prompt@, and so never runs the strategy;
'Control.Monad.Prompt.Effect.improve' is flat as well. A program that means to use
the strategy must call @Promptable.prompt \@Musician@, qualified as in
@import Control.Monad.Prompt.Promptable qualified as Promptable@.

== The strategy, call by call

'prompt' for @'Generically' a@ builds an @a@ in several requests, all inside one
snapshot of the context:

1. Only for a type with several constructors: a message tells the model it is
   about to build a value of @a@, described by 'description', and asks which
   constructor to build it with. The model is asked for a 'ConstructorChoice', an
   enum of the constructor names. The reply stays in the context. A type with
   one constructor skips this step.
2. A message tells the model it is about to build a value of @a@, described by
   'description', field by field (after a choice: that it chose this constructor),
   and the model is asked for a 'Cue': a short,
   specific steer that every field will follow. The reply stays in the context.
   A constructor without fields skips this step and the next.
3. For each field in order, a message names the field (a record's field name,
   otherwise the constructor's name and the field's position, and for a type with
   several constructors always the constructor too), and the field
   type's __own__ 'Control.Monad.Prompt.Promptable.prompt' is called. Its
   answer stays in the context, so each field sees the earlier ones. A field type
   whose own @prompt@ scopes its context (and so leaves no answer behind) has its
   answer appended by the strategy: whenever the context did not grow, or does not
   end in an 'AssistantMessage'. The strategy cannot tell whether a trailing
   'AssistantMessage' is really that field's answer, so an override that drops
   its answer yet appends some other 'AssistantMessage' last goes unnoticed.
4. The fields are assembled into an @a@, a /draft/.
5. The draft goes through 'Control.Monad.Prompt.Effect.improve', unconditionally. A
   @fine@ verdict keeps the draft, an @improved@ one replaces it.
6. The context is restored to the snapshot, and one 'AssistantMessage' holding the
   final value's JSON is appended: the caller's context ends as after a flat
   'Control.Monad.Prompt.Promptable.prompt', that is, what it was plus the value.

The /cue/ is there so that fields asked for one by one still read as one value:
a tone or a setting that the first field's request would otherwise leave to
chance, and the later ones inherit. 'improve' is there to catch what a
field-by-field build cannot see: a pair of fields that do not fit together. It is
checked against the type's own 'Property', the one 'Surveyable' gives it: the
structural one (the properties of the fields) by default, and only the custom
one if the type replaced it (see the @Either … ('Structural' X)@ pattern in
'Surveyable'). A draft that violates a property is never kept on a @fine@ verdict:
'improve' retries that.

== Cost

Per generic node: one choice (sums only), one cue, one request per field
(recursively, for a field that is itself generic) and one 'improve'. That is
@n + 2@ requests for a record with @n@ fields (cue, fields, improve), @n + 3@
for a constructor with @n > 0@ fields of a type with several constructors (choice,
cue, fields, improve) and 2 for a constructor without fields (choice, improve);
and one more for every retry. Flat leaves cost one request each. So the example
@Musician@ costs 4 requests, and a @Fruit@ 4 (choice, cue, the one field,
improve), where a flat 'Control.Monad.Prompt.Effect.prompt' costs 1.

== Leaves flat, composites generic

Derive leaves and newtypes (@Name@, @City@, a unit-carrying @Double@) flat, with
a bare @instance Promptable@: asking for them part by part would pay a cue and an
@improve@ for one scalar. Reach for @Generically@ for the records and sums whose
parts must cohere, such as a musician and the city they are from, or a fruit and
its measurements, where the cue and the earlier fields steer the later ones.

== Containers are prompted whole

A field of type @[a]@, @'NonEmpty' a@ or @'Maybe' a@ is prompted whole, in one
request for the container (issue #24), through its flat 'Promptable' instance;
the strategy does not go element by element. The properties of the elements are
therefore not checked, nor told to the model, as for any container field (issue
#18, see "Data.Shroom.Class").

== Recursive types hang, and fail against Claude

A directly or mutually recursive type (@data Expr = Lit Int | Neg Expr@: a field
of its own type that is not under a list, 'NonEmpty' or 'Maybe', or one that
reaches itself through another type's bare instance) with a bare
@instance Surveyable@ hangs under the structural default, on every backend and
before any request is sent: enumerating its structural 'Property' never ends.
Such a type must opt out of the default with @type Property X = Void@ (or a custom
'Property'), see 'Surveyable'.

Independently of that, Anthropic's structured outputs do not support recursive
schemas. A type that mentions itself (a tree whose children are trees, say)
therefore fails against Claude even with the opt-out, in the generic strategy as
in a flat request, which sends the same schema.

== Which types

Every type with 'SOP.Generic' and 'SOP.HasDatatypeInfo' whose every field type is
'Promptable'. A sum's flat schema, which 'Control.Monad.Prompt.Effect.improve' sends,
has its @oneOf@ rewritten to @anyOf@ on the wire: see
'Control.Monad.Prompt.Backend.schemaWithDefs'.

== Orphan instances

This module holds two orphans, each commented where it is defined.
-}
module Control.Monad.Prompt.Generic (
  Cue (..),
  ConstructorChoice,
) where

-- base
import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Data.Void (Void)
import GHC.Generics (Generically (..))

-- text
import Data.Text (Text)
import Data.Text qualified as T

-- aeson
import Data.Aeson (FromJSON (..), ToJSON (..), Value (String), withText)

-- openapi3
import Data.OpenApi (NamedSchema (..), OpenApiType (..), Schema (..), ToSchema (..))

-- generics-sop
import Generics.SOP (All, All2, Code, ConstructorInfo, I (..), K (..), NP (..), NS (..), SList (..), SOP (..), constructorInfo, constructorName, datatypeInfo, datatypeName, sList, to)
import Generics.SOP qualified as SOP

-- effectful
import Effectful (Eff, (:>))
import Effectful.State.Static.Local (State, get, put)

-- shroom
import Control.Monad.Prompt.Backend (ContextItem (..))
import Control.Monad.Prompt.Effect (Prompt, addContextItem, improve)
import Control.Monad.Prompt.Promptable (Promptable)
import Control.Monad.Prompt.Promptable qualified as Promptable
import Control.Monad.Prompt.Schema (encodeJson)
import Data.Shroom.Class

-- * The cue

{- | A short, specific steer that every field of a value under construction
follows: a tone, a setting, a constraint. The strategy asks the model for it
before the first field, once the constructor is known.
-}
newtype Cue = Cue Text
  deriving stock (Eq, Show)
  deriving newtype (ToJSON, FromJSON, ToSchema)

instance Describable Cue where
  describeType _ = "A short, specific steer, in one or two sentences: the tone, setting or constraint that every field of the value being built should follow."

instance Surveyable Cue where
  type Property Cue = Void

-- | Flat, like the base types.
instance Promptable Cue

-- * The constructor choice

{- | The constructor of an @a@ to build, one of the constructors of @a@. The first
thing the strategy asks the model for when @a@ has several constructors.

On the wire it is a string, one of the constructor names, and nothing else is
accepted; its schema is a string enum named after @a@, so the choices of two
sum types do not collide.
-}
newtype ConstructorChoice a = ConstructorChoice (NS (K ()) (Code a))

-- | The names of the constructors of @a@, in order.
constructorNames :: forall a. (SOP.HasDatatypeInfo a) => Proxy a -> [Text]
constructorNames prx = go (constructorInfo (datatypeInfo prx))
  where
    go :: NP ConstructorInfo xss -> [Text]
    go Nil = []
    go (c :* cs) = T.pack (constructorName c) : go cs

-- | The name of the chosen constructor.
choiceName :: NP ConstructorInfo xss -> NS (K ()) xss -> Text
choiceName (c :* _) (Z _) = T.pack (constructorName c)
choiceName (_ :* cs) (S choice) = choiceName cs choice
choiceName Nil choice = case choice of {}

-- | The choice naming a constructor, if there is one of that name.
choiceFromName :: NP ConstructorInfo xss -> Text -> Maybe (NS (K ()) xss)
choiceFromName Nil _ = Nothing
choiceFromName (c :* cs) name
  | T.pack (constructorName c) == name = Just (Z (K ()))
  | otherwise = S <$> choiceFromName cs name

instance (SOP.HasDatatypeInfo a) => ToJSON (ConstructorChoice a) where
  toJSON (ConstructorChoice choice) = String (choiceName (constructorInfo (datatypeInfo (Proxy @a))) choice)

instance (SOP.HasDatatypeInfo a) => FromJSON (ConstructorChoice a) where
  parseJSON = withText "ConstructorChoice" $ \name ->
    case choiceFromName (constructorInfo (datatypeInfo (Proxy @a))) name of
      Just choice -> pure (ConstructorChoice choice)
      Nothing -> fail ("not the name of a constructor: " <> T.unpack name <> ", expected one of " <> T.unpack (T.intercalate ", " (constructorNames (Proxy @a))))

instance (Typeable a, SOP.HasDatatypeInfo a) => ToSchema (ConstructorChoice a) where
  declareNamedSchema _ =
    pure $
      NamedSchema
        (Just (T.pack (datatypeName (datatypeInfo (Proxy @a))) <> "Choice"))
        mempty
          { _schemaType = Just OpenApiString
          , _schemaEnum = Just (String <$> constructorNames (Proxy @a))
          }

instance (SOP.HasDatatypeInfo a, Describable a) => Describable (ConstructorChoice a) where
  describeType _ =
    "A choice of which kind of the type described below to build, made by naming one of its constructors, exactly as written.\n"
      <> describeType (Proxy @a)

instance (SOP.HasDatatypeInfo a, Describable a) => Surveyable (ConstructorChoice a) where
  type Property (ConstructorChoice a) = Void

-- | Flat, like the 'Cue'.
instance (Typeable a, SOP.HasDatatypeInfo a, Describable a) => Promptable (ConstructorChoice a)

-- * The strategy

{- | The strategy, see the module header. Written for @a@ itself; the
'Promptable' instance for 'Generically' wraps it.
-}
promptGeneric ::
  forall a es.
  ( Prompt :> es
  , State [ContextItem] :> es
  , Surveyable a
  , ToSchema a
  , FromJSON a
  , SOP.Generic a
  , SOP.HasDatatypeInfo a
  , All2 Promptable (Code a)
  ) =>
  Eff es a
promptGeneric = do
  (saved :: [ContextItem]) <- get
  let parent = description (Proxy @a)
      choose = (\(ConstructorChoice choice) -> choice) <$> Promptable.prompt @(ConstructorChoice a)
  built <- promptConstructors parent choose (constructorInfo (datatypeInfo (Proxy @a)))
  let draft = to (SOP built)
  final <- fromMaybe draft <$> improve draft
  put (saved <> [AssistantMessage (encodeJson final)])
  pure final

{- | Build the fields of one of the constructors. A single constructor is the
type itself and needs no choice; with several, the model is first told what is
being built and asked which constructor (the action @choose@ does the asking), and
then the fields of that constructor are built. @parent@ is the description of the
whole type.
-}
promptConstructors ::
  forall xss es.
  (Prompt :> es, State [ContextItem] :> es, All2 Promptable xss) =>
  Text ->
  Eff es (NS (K ()) xss) ->
  NP ConstructorInfo xss ->
  Eff es (NS (NP I) xss)
promptConstructors parent choose constructors = case constructors of
  constructor :* Nil -> Z <$> promptConstructor parent False constructor
  _ -> do
    -- Persistent context, not 'withContext': that would drop every answer.
    addContextItem $
      UserMessage $
        "You are about to build a value of the type described below. First choose which of its constructors to build it with; then its fields are built one by one.\n"
          <> parent
    choice <- choose
    promptChosen parent constructors choice

-- | Build the fields of the chosen one of several constructors.
promptChosen ::
  forall xss es.
  (Prompt :> es, State [ContextItem] :> es, All2 Promptable xss) =>
  Text ->
  NP ConstructorInfo xss ->
  NS (K ()) xss ->
  Eff es (NS (NP I) xss)
promptChosen parent (constructor :* _) (Z _) = Z <$> promptConstructor parent True constructor
promptChosen parent (_ :* constructors) (S choice) = S <$> promptChosen parent constructors choice
promptChosen _ Nil choice = case choice of {}

{- | The product strategy for one constructor: a message and the 'Cue', then its
fields. A constructor without fields is built at once. The flag says whether the
constructor was chosen among several, which the messages then say.
-}
promptConstructor ::
  forall xs es.
  (Prompt :> es, State [ContextItem] :> es, All Promptable xs) =>
  Text ->
  Bool ->
  ConstructorInfo xs ->
  Eff es (NP I xs)
promptConstructor parent chosen constructor = case sList :: SList xs of
  SNil -> pure Nil
  SCons -> do
    addContextItem $
      UserMessage $
        if chosen
          then "You chose the constructor " <> T.pack (constructorName constructor) <> ". Before its first field, give one short, specific steer that every field will follow."
          else
            "You are about to build a value of the type described below, field by field.\n"
              <> parent
              <> "\nBefore the first field, give one short, specific steer that every field will follow."
    _cue <- Promptable.prompt @Cue
    promptFields (fieldLabel chosen constructor) 0

{- | Prompt each field of one constructor in order, each through its own type's
'Promptable.prompt', after a message naming it. The label function says how to
name the field at a (zero-based) position.
-}
promptFields ::
  forall xs es.
  (Prompt :> es, State [ContextItem] :> es, All Promptable xs) =>
  (Int -> Text) ->
  Int ->
  Eff es (NP I xs)
promptFields label i = case sList :: SList xs of
  SNil -> pure Nil
  SCons -> do
    addContextItem (UserMessage ("Next, produce " <> label i <> " of the value."))
    (before :: [ContextItem]) <- get
    value <- Promptable.prompt
    (after :: [ContextItem]) <- get
    -- A field whose own @prompt@ scopes its context leaves no answer behind;
    -- later fields must still see it. An answer is taken to be a context that
    -- grew and ends in an 'AssistantMessage'.
    unless (length after > length before && endsInAssistantMessage after) $
      addContextItem (AssistantMessage (encodeJson value))
    rest <- promptFields label (i + 1)
    pure (I value :* rest)

-- | Whether the last item of a context is an 'AssistantMessage'.
endsInAssistantMessage :: [ContextItem] -> Bool
endsInAssistantMessage ctx = case reverse ctx of
  AssistantMessage _ : _ -> True
  _ -> False

{- | The name of the field at a position: the record field's, else the
constructor's and the position. When the constructor was chosen among several,
a record field's name is followed by the constructor's as well.
-}
fieldLabel :: Bool -> ConstructorInfo xs -> Int -> Text
fieldLabel chosen constructor i = case fieldNameAt i constructor of
  Just name
    | chosen -> "the field " <> name <> " of constructor " <> conName
    | otherwise -> "the field " <> name
  Nothing -> "field " <> T.pack (show (i + 1)) <> " of constructor " <> conName
  where
    conName = T.pack (constructorName constructor)

-- * The instances

{- | Orphan. @openapi3@ has no 'ToSchema' for 'Generically', and it belongs there
upstream; until then it is here. It forwards to @a@: the strategy only ever
uses the schema of @a@ itself, and this instance exists to satisfy the
superclass of 'Promptable'.
-}
instance (ToSchema a) => ToSchema (Generically a) where
  declareNamedSchema _ = declareNamedSchema (Proxy @a)

{- | Orphan. It cannot sit next to the class in
"Control.Monad.Prompt.Promptable": the strategy calls that class's own
'Promptable.prompt' on every field and 'improve' from
"Control.Monad.Prompt.Effect", and keeping the strategy in a module of its own is
worth one orphan. 'Describable' and 'Surveyable' for 'Generically' are not
orphans: they are in "Data.Shroom.Class".

The method is what @deriving via@ coerces; the context is mostly about @a@,
and @ToJSON (Generically a)@ and @FromJSON (Generically a)@ are @aeson@'s
generic ones, needed by the superclasses.
-}
instance
  ( Surveyable a
  , ToSchema a
  , FromJSON a
  , ToJSON (Generically a)
  , FromJSON (Generically a)
  , SOP.Generic a
  , SOP.HasDatatypeInfo a
  , All2 Promptable (Code a)
  ) =>
  Promptable (Generically a)
  where
  prompt = Generically <$> promptGeneric @a
