{- | The package environment the test suite is built in, captured while it is
built, so that the test can hand it to a @ghc@ of its own.

Cabal does not tell a test which package databases and packages it was built
against: it passes them to the compiler as flags. The compiler is a Haskell
process that runs a Template Haskell splice in itself, so the splice can read
those flags from its own arguments.
-}
module PackageFlags (capturePackageFlags) where

-- base
import Data.List (isPrefixOf)
import GHC.ResponseFile (unescapeArgs)
import System.Environment (getArgs)

-- template-haskell
import Language.Haskell.TH (Exp, Q, runIO)
import Language.Haskell.TH.Syntax (lift)

{- | A splice of type @[String]@: the package flags (@-package-db@,
@-package-id@, @-hide-all-packages@, …) among the arguments of the compiler
process that runs it, in order. Cabal hands them over in a response file
(@\@file@), which exists as long as the compiler runs, so it is expanded.
-}
capturePackageFlags :: Q Exp
capturePackageFlags = do
  args <- runIO (getArgs >>= fmap concat . traverse expand)
  lift (packageFlags args)
  where
    expand ('@' : file) = unescapeArgs <$> readFile file
    expand arg = pure [arg]

-- | The package flags among compiler arguments, each with its separate argument if it has one.
packageFlags :: [String] -> [String]
packageFlags (flag : arg : args)
  | flag `elem` withSeparateArgument = flag : arg : packageFlags args
packageFlags (flag : args)
  | any (`isPrefixOf` flag) packagePrefixes = flag : packageFlags args
  | otherwise = packageFlags args
packageFlags [] = []

-- | The flags that take their argument as the next word, not after an @=@.
withSeparateArgument :: [String]
withSeparateArgument =
  ["-package", "-package-id", "-package-db", "-package-env", "-hide-package", "-ignore-package", "-trust", "-distrust"]

-- | Every package flag starts with one of these.
packagePrefixes :: [String]
packagePrefixes =
  [ "-package"
  , "-hide-package"
  , "-hide-all-packages"
  , "-ignore-package"
  , "-clear-package-db"
  , "-global-package-db"
  , "-user-package-db"
  , "-no-user-package-db"
  , "-trust"
  , "-distrust"
  ]
