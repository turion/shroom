{-# LANGUAGE TemplateHaskell #-}

{- | Template Haskell utilities for deriving 'Describable' instances from Haddock
documentation comments.
-}
module Control.Monad.Prompt.TH (deriveDescribable, deriveDescribableExp) where

-- base
import Data.Kind qualified as Kind

-- template-haskell
import Language.Haskell.TH

-- text
import Data.Text (pack)

-- shroom
import Data.Shroom.Class (Describable (describeType, referencedTypes), reach)

{- | Generate a complete 'Describable' instance for a type, deriving
'describeType' from its Haddock documentation comment and
'referencedTypes' from its fields.  Use this when
you don't need to customise the description:

@
-- | A user with a name and an email address.
data User = User { userName :: Text, userEmail :: Text }

\$(deriveDescribable ''User)
@

This generates:

@
instance Describable User where
  describeType = \_ -> pack "A user with a name and an email address.\\n..."
  referencedTypes _ = reach \@Text <> (reach \@Text <> mempty)
@

'referencedTypes' is the concatenation, in declaration order, of @reach \@t@ for
the type @t@ of every field of every constructor, record and positional alike. So
the types of the fields reach the model through 'Data.Shroom.Class.description':
each is described once, in a section after the examples, however often and however
deeply it is used.

__Every field type must have a 'Describable' instance__, from an earlier splice or
the same one; a field type without one is a compile-time error (\"No instance for
'Describable' …\"). Splice order therefore matters. Mutually recursive types
are derived in a single splice, @\$(concat \<$\> traverse deriveDescribable [''A, ''B])@; a
self-recursive type needs nothing special.

A parameterised type derives too, with one @'Describable' a@ constraint for each type
variable of kind @Type@:

@
-- | One of two alternatives.
data Choice a = Pick a | Skip

\$(deriveDescribable ''Choice)
@

generates @instance ('Describable' a) => 'Describable' (Choice a)@, where a field of type
@a@ contributes @reach \@a@, so that @Choice Fuel@ makes @Fuel@ reachable. A type
variable of any other kind is a compile-time error: write the instance by hand
then. The generated code uses type applications and the instance head's type
variables, so the module needs @TypeApplications@ and @ScopedTypeVariables@ (both
in @GHC2021@ and @GHC2024@).

To override 'describeType' manually, write the 'Describable' instance by hand.
You can use 'deriveDescribableExp' for the body, and modify it as needed, but
then also write 'referencedTypes'.

@
instance Describable MyType where
  describeType = \$(deriveDescribableExp ''MyType)
  referencedTypes _ = reach \@Inner
@

This requires the module to be compiled with the @-haddock@ GHC flag,
which is set automatically when using this package's build configuration.

__Error__: A compile-time error is raised if no Haddock documentation is found.
-}
deriveDescribable :: Name -> Q [Dec]
deriveDescribable name = do
  body <- deriveDescribableExp name
  (typeVars, fieldTypes) <- getTypeShape name
  let describeTypeClause = ValD (VarP 'describeType) (NormalB body) []
      -- referencedTypes = \_ -> reach @T1 <> (reach @T2 <> (... <> mempty)),
      -- with no list literal, which OverloadedLists would make ambiguous
      referencedTypesBody =
        LamE
          [WildP]
          ( foldr
              (\fieldType rest -> InfixE (Just (AppTypeE (VarE 'reach) fieldType)) (VarE '(<>)) (Just rest))
              (VarE 'mempty)
              fieldTypes
          )
      referencedTypesClause = ValD (VarP 'referencedTypes) (NormalB referencedTypesBody) []
      instanceHead = foldl AppT (ConT name) (VarT <$> typeVars)
      context = [AppT (ConT ''Describable) (VarT v) | v <- typeVars]
  pure
    [ InstanceD
        Nothing
        context
        (AppT (ConT ''Describable) instanceHead)
        [describeTypeClause, referencedTypesClause]
    ]

{- | The type variables of a type, which must all be of kind @Type@, and the types of all fields of all
its constructors, in declaration order.
-}
getTypeShape :: Name -> Q ([Name], [Type])
getTypeShape name = do
  info <- reify name
  (binders, cons) <- case info of
    TyConI (DataD _ _ tvs _ cs _) -> pure (tvs, cs)
    TyConI (NewtypeD _ _ tvs _ c _) -> pure (tvs, [c])
    _ -> pure ([], [])
  typeVars <- mapM checkKind binders
  pure (typeVars, concatMap fieldTypesOf cons)
  where
    checkKind (PlainTV v _) = pure v
    checkKind (KindedTV v _ k)
      | isTypeKind k = pure v
      | otherwise =
          fail $
            "deriveDescribable: the type variable '"
              <> nameBase v
              <> "' of '"
              <> nameBase name
              <> "' has the kind "
              <> pprint k
              <> ", but only type variables of kind Type are supported.\n"
              <> "Write the Describable instance of '"
              <> nameBase name
              <> "' by hand instead, with deriveDescribableExp for describeType and referencedTypes of your own."

    isTypeKind StarT = True
    isTypeKind (ConT k) = k == ''Kind.Type
    isTypeKind _ = False

    fieldTypesOf (NormalC _ bts) = snd <$> bts
    fieldTypesOf (RecC _ vbts) = [t | (_, _, t) <- vbts]
    fieldTypesOf (InfixC (_, t1) _ (_, t2)) = [t1, t2]
    fieldTypesOf (ForallC _ _ c) = fieldTypesOf c
    fieldTypesOf (GadtC _ bts _) = snd <$> bts
    fieldTypesOf (RecGadtC _ vbts _) = [t | (_, _, t) <- vbts]

{- | Produce the 'describeType' method body for a type, using its Haddock
documentation comment.  Use this inside a manually written
'Describable' instance:

@
instance Describable MyType where
  describeType = \$(deriveDescribableExp ''MyType)
  referencedTypes _ = reach \@Inner
@

__This returns the 'describeType' body only.__ It does not collect the referenced types:
a hand-written instance must write 'referencedTypes' itself, or its field types are not
described to the model. For a wrapper that is @referencedTypes _ = reach \@a@; for a
record, the @reach \@t@ of each of its field types, as 'deriveDescribable' generates.

For a type with several constructors, each constructor is listed as
@- Constructor: doc@ (by name alone if undocumented), followed by its documented
record fields indented beneath it.  For a single-constructor type, the documented
record fields are listed directly.  Documentation of positional fields is not collected.

Prefer 'deriveDescribable' for the common case where a full instance is needed.
-}
deriveDescribableExp :: Name -> Q Exp
deriveDescribableExp name = do
  mdoc <- getDoc (DeclDoc name)
  doc <- case mdoc of
    Nothing ->
      fail $
        "deriveDescribable: No Haddock documentation found for '"
          <> nameBase name
          <> "'.\n"
          <> "Make sure to:\n"
          <> "  1. Add a Haddock comment above the type declaration (-- | ...).\n"
          <> "  2. Compile with the -haddock GHC flag (e.g. add 'ghc-options: -haddock' to your .cabal file).\n"
          <> "  3. Separate the type declaration from this splice with $(pure []) if they are in the same module."
    Just d -> pure d
  constructorDocs <- getConstructorDocs name
  -- Build: \_ -> pack "<doc>"
  -- Haddock docs have a leading space, no trailing newline, and escape "/" as "\/"
  let unescapeHaddock [] = []
      unescapeHaddock ('\\' : '/' : rest) = '/' : unescapeHaddock rest
      unescapeHaddock (c : rest) = c : unescapeHaddock rest
      normalizeDoc = (++ "\n") . dropWhile (== ' ') . unescapeHaddock
      renderField indent (fn, fd) = indent <> "- " <> fn <> ": " <> normalizeDoc fd
      -- A sum lists every constructor (by name alone if undocumented),
      -- with its documented fields indented beneath it.
      renderConstructor (cn, mcd, fields) =
        "- "
          <> cn
          <> maybe "\n" ((": " <>) . normalizeDoc) mcd
          <> concatMap (renderField "  ") fields
      -- A single constructor is the type itself, so its fields are listed directly.
      constructorLines = case constructorDocs of
        [(_, _, fields)] -> concatMap (renderField "") fields
        cons -> concatMap renderConstructor cons
      fullDoc = normalizeDoc doc <> constructorLines
      docLit = litE (stringL fullDoc)
  [|\_ -> pack $docLit|]

{- | Collect, for each constructor of a type, its name, its Haddock (if any), and
the (fieldName, doc) pairs of its documented record fields.  Positional fields'
docs are not collected.
-}
getConstructorDocs :: Name -> Q [(String, Maybe String, [(String, String)])]
getConstructorDocs name = do
  info <- reify name
  let cons = case info of
        TyConI (DataD _ _ _ _ cs _) -> cs
        TyConI (NewtypeD _ _ _ _ c _) -> [c]
        _ -> []
  mapM collect cons
  where
    collect con = do
      let cn = conName con
      cdoc <- getDoc (DeclDoc cn)
      pairs <- mapM (\fn -> (nameBase fn,) <$> getDoc (DeclDoc fn)) (recFields con)
      pure (nameBase cn, cdoc, [(fn, d) | (fn, Just d) <- pairs])

    conName (NormalC n _) = n
    conName (RecC n _) = n
    conName (InfixC _ n _) = n
    conName (ForallC _ _ c) = conName c
    conName (GadtC (n : _) _ _) = n
    conName (GadtC [] _ _) = mkName ""
    conName (RecGadtC (n : _) _ _) = n
    conName (RecGadtC [] _ _) = mkName ""

    recFields (RecC _ fields) = [fn | (fn, _, _) <- fields]
    recFields (ForallC _ _ c) = recFields c
    recFields (RecGadtC _ fields _) = [fn | (fn, _, _) <- fields]
    recFields _ = []
