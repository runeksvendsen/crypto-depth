module Main where

import Prelude
import CryptoDepth.Internal.Types
import Test.HUnit
import qualified Data.List.NonEmpty  as NE
import Test.Hspec.Expectations.Pretty


main :: IO Counts
main = runTestTT tests

tests :: Test
tests = TestList
  [ TestLabel "id"  unit_id
  , TestLabel "id2" unit_id2
  , TestLabel "id3" unit_id3
  , TestLabel "1-2" unit_12
  , TestLabel "2-2" unit_22
  , TestLabel "1-3" unit_13
  , TestLabel "2-3" unit_23
  ]

unit_id :: Test
unit_id =
    TestCase $ groupVenues (NE.fromList input) `shouldBe` NE.fromList output
  where
    output = [ ExchangePath "venue1" "a" "b" []
             ]
    input = [ ("a", "venue1", "b")
            ]

unit_id2 :: Test
unit_id2 =
    TestCase $ groupVenues (NE.fromList input) `shouldBe` NE.fromList output
  where
    output = [ ExchangePath "venue1" "a" "b" []
             , ExchangePath "venue2" "b" "c" []
             ]
    input = [ ("a", "venue1", "b")
            , ("b", "venue2", "c")
            ]


unit_id3 :: Test
unit_id3 =
    TestCase $ groupVenues (NE.fromList input) `shouldBe` NE.fromList output
  where
    output = [ ExchangePath "venue1" "a" "b" []
             , ExchangePath "venue2" "b" "c" []
             , ExchangePath "venue3" "c" "d" []
             ]
    input = [ ("a", "venue1", "b")
            , ("b", "venue2", "c")
            , ("c", "venue3", "d")
            ]

unit_12 :: Test
unit_12 =
    TestCase $ groupVenues (NE.fromList input) `shouldBe` NE.fromList output
  where
    output = [ ExchangePath "venue1" "b" "c" []
             , ExchangePath "venue2" "c" "e" ["d"]
             ]
    input = [ ("b", "venue1", "c")
            , ("c", "venue2", "d")
            , ("d", "venue2", "e")
            ]

unit_22 :: Test
unit_22 =
    TestCase $ groupVenues (NE.fromList input) `shouldBe` NE.fromList output
  where
    output = [ ExchangePath "venue1" "a" "c" ["b"]
             , ExchangePath "venue2" "c" "e" ["d"]
             ]
    input = [ ("a", "venue1", "b")
            , ("b", "venue1", "c")
            , ("c", "venue2", "d")
            , ("d", "venue2", "e")
            ]

unit_13 :: Test
unit_13 =
    TestCase $ groupVenues (NE.fromList input) `shouldBe` NE.fromList output
  where
    output = [ ExchangePath "venue1" "b" "c" []
             , ExchangePath "venue2" "c" "f" ["d", "e"]
             ]
    input = [ ("b", "venue1", "c")
            , ("c", "venue2", "d")
            , ("d", "venue2", "e")
            , ("e", "venue2", "f")
            ]

unit_23 :: Test
unit_23 =
    TestCase $ groupVenues (NE.fromList input) `shouldBe` NE.fromList output
  where
    output = [ ExchangePath "venue1" "a" "c" ["b"]
             , ExchangePath "venue2" "c" "f" ["d", "e"]
             ]
    input = [ ("a", "venue1", "b")
            , ("b", "venue1", "c")
            , ("c", "venue2", "d")
            , ("d", "venue2", "e")
            , ("e", "venue2", "f")
            ]
