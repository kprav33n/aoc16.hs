import Test.Tasty
import Test.Tasty.HUnit

import Lib

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [taxicabDistanceTests, makeInstructionsTests, hqDistanceTests]

taxicabDistanceTests :: TestTree
taxicabDistanceTests = testGroup "taxicabDistance unit test"
  [ testCase "No instructions" $
    taxicabDistance [] @?= 0

  , testCase "R2, L3" $
    taxicabDistance [R 2 , L 3] @?= 5

  , testCase "R2, R2, R2" $
    taxicabDistance [R 2, R 2, R 2] @?= 2

  , testCase "R5, L5, R5, R3" $
    taxicabDistance [R 5, L 5, R 5, R 3] @?= 12
  ]

makeInstructionsTests :: TestTree
makeInstructionsTests = testGroup "makeInstructions unit test"
  [ testCase "No instructions" $
    makeInstructions "" @?= []

  , testCase "R2, L3" $
    makeInstructions "R2, L3" @?= [R 2 , L 3]

  , testCase "R5, L5, R5, R3" $
    makeInstructions "R5, L5, R5, R3" @?= [R 5, L 5, R 5, R 3]
  ]

hqDistanceTests :: TestTree
hqDistanceTests = testGroup "hqDistance unit test"
  [ testCase "R8, R4, R4, R8" $
    hqDistance [R 8, R 4, R 4, R 8] @?= 4
  ]
