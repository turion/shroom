{-# LANGUAGE TemplateHaskell #-}

{- | Tests that a type recursive through its structural properties does not
compile with a bare @instance Surveyable@.

A declaration that must be rejected cannot be a test inside the test suite, which
would then not build, so each one lives in a module of its own in @test/fixtures@
that the test compiles with @ghc -fno-code@ and inspects the error of. The
compiler gets the package environment the test suite was built in, see
"PackageFlags", so a fixture sees the same @shroom-class@, and the same
dependencies, as the rest of the tests. The tests expect to run with the package
directory as their working directory, as @cabal test@ runs them.
-}
module Rejection (rejectionTests) where

-- base
import Data.List (isInfixOf)
import System.Exit (ExitCode (..))

-- process
import System.Process (readProcessWithExitCode)

-- tasty
import Test.Tasty (TestTree, testGroup)

-- tasty-hunit
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

-- shroom-class-test
import PackageFlags (capturePackageFlags)

-- | The package flags of the compiler that built this module.
packageFlags :: [String]
packageFlags = $(capturePackageFlags)

-- | What compiling a fixture without generating code gave: the exit code, and the output of the compiler.
compileFixture :: String -> IO (ExitCode, String)
compileFixture name = do
  (code, out, err) <- readProcessWithExitCode "ghc" (["-fno-code"] <> packageFlags <> ["test/fixtures/" <> name <> ".hs"]) ""
  pure (code, out <> err)

{- | The tests: the control compiles, so that a rejection is not the compiler failing for another reason, and each
recursive fixture is rejected with a message that names the type that recurs, the opt-out and what it drops.
-}
rejectionTests :: TestTree
rejectionTests =
  testGroup
    "recursion through the structural default is a compile error"
    [ testCase "control: a bare instance and an opted-out recursive type compile" $ do
        (code, out) <- compileFixture "Accepted"
        assertBool ("the control fixture does not compile, so the package flags the compiler is given are wrong:\n" <> out) (code == ExitSuccess)
    , rejected "RecTree" "RecTree"
    , rejected "Mutual" "A"
    , rejected "Mutual" "B"
    , rejected "Grouping" "Grouping"
    ]

{- | The fixture does not compile, and the error says that @ty@ is recursive, gives
@type Property ty = Void@ as the way out and says that it drops the properties of the other fields.
-}
rejected :: String -> String -> TestTree
rejected fixture ty = testCase (fixture <> ": a bare instance for " <> ty <> " is rejected") $ do
  (code, out) <- compileFixture fixture
  let output = unwords (words out)
      expect fragment = assertBool ("the compiler's output does not contain " <> show fragment <> ":\n" <> out) (fragment `isInfixOf` output)
  case code of
    ExitSuccess -> assertFailure ("the fixture compiles, but should be rejected:\n" <> out)
    ExitFailure _ -> do
      expect (ty <> " is recursive through its structural properties")
      expect ("type Property " <> ty <> " = Void")
      expect "which drops the properties of its other fields too"
