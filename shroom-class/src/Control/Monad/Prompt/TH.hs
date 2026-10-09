{-# LANGUAGE TemplateHaskell #-}

{- | Template Haskell utilities for deriving 'Describable' instances from Haddock
documentation comments.
-}
module Control.Monad.Prompt.TH (deriveDescribable, deriveDescribableExp) where

-- template-haskell
import Language.Haskell.TH

-- text
import Data.Text (pack)

-- shroom
import Data.Shroom.Class (Describable (describeType))

{- | Generate a complete 'Describable' instance for a type, deriving
'describeType' from its Haddock documentation comment.  Use this when
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
@

To override 'describeType' manually, write the 'Describable' instance by hand.
You can use 'deriveDescribableExp' for the body, and modify it as needed.

@
instance Describable MyType where
  describeType = \$(deriveDescribableExp ''MyType)
@

This requires the module to be compiled with the @-haddock@ GHC flag,
which is set automatically when using this package's build configuration.

__Error__: A compile-time error is raised if no Haddock documentation is found.
-}
deriveDescribable :: Name -> Q [Dec]
deriveDescribable name = do
  body <- deriveDescribableExp name
  let describeTypeClause = ValD (VarP 'describeType) (NormalB body) []
  pure
    [ InstanceD
        Nothing
        []
        (AppT (ConT ''Describable) (ConT name))
        [describeTypeClause]
    ]

{- | Produce the 'describeType' method body for a type, using its Haddock
documentation comment.  Use this inside a manually written
'Describable' instance:

@
instance Describable MyType where
  describeType = \$(deriveDescribableExp ''MyType)
@

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
