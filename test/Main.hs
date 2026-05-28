module Main (main) where

-- base
import Data.Proxy (Proxy (..))

-- containers
import Data.Set qualified as S

-- tasty
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- shroom
import Data.Describe (Describe (..))

-- test
import Types

-- * Tests

main :: IO ()
main =
  defaultMain $
    testGroup
      "deriveDescribeType"
      [ testCase "User description comes from Haddock comment" $
          describeType (Proxy @User)
            @?= "A user with a name and an email address.\n"
              <> "- userName: The user's full name.\n"
              <> "- userEmail: The user's email address.\n"
      , testCase "Counter description comes from Haddock comment" $
          describeType (Proxy @Counter)
            @?= "An integer counter that keeps track of how many times an event has occurred.\n"
      , testCase "Coordinate description comes from Haddock comment" $
          describeType (Proxy @Coordinate)
            @?= "A geographic coordinate expressed as a latitude/longitude pair.\n"
              <> "- latitude: The latitude in degrees.\n"
              <> "- longitude: The longitude in degrees.\n"
      , testCase "describeProperties works for UserEmailNotEmpty" $
          describeProperties (Proxy @User) UserEmailNotEmpty
            @?= "The email address is not empty."
      , testCase "failingProperties catches empty email" $
          failingProperties (User "Alice" "") @?= S.singleton UserEmailNotEmpty
      , testCase "failingProperties passes non-empty email" $
          failingProperties (User "Alice" "alice@example.com") @?= S.empty
      ]
