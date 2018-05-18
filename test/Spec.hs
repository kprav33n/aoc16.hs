import Test.Tasty
import Test.Tasty.HUnit

import Day01
import Day02

main :: IO ()
main = defaultMain tests
tests :: TestTree
tests = testGroup "Tests" [day01Tests, day02Tests]

day01Tests :: TestTree
day01Tests = testGroup "Day01 tests" [taxicabDistanceTests, makeInstructionsTests, hqDistanceTests]

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
    Day01.makeInstructions "" @?= []

  , testCase "R2, L3" $
    Day01.makeInstructions "R2, L3" @?= [R 2 , L 3]

  , testCase "R5, L5, R5, R3" $
    Day01.makeInstructions "R5, L5, R5, R3" @?= [R 5, L 5, R 5, R 3]
  ]

hqDistanceTests :: TestTree
hqDistanceTests = testGroup "hqDistance unit test"
  [ testCase "R8, R4, R4, R8" $
    hqDistance [R 8, R 4, R 4, R 8] @?= 4
  ]

day02Tests :: TestTree
day02Tests = testGroup "Day02 tests" [bathroomCodeTests]

bathroomCodeTests :: TestTree
bathroomCodeTests = testGroup "bathroomCode unit test"
  [ testCase "Simple code" $
    bathroomCode (Day02.makeInstructions "ULL\nRRDDD\nLURDL\nUUUUD") @?= "1985"

  , testCase "Diamond code" $
    bathroomDiamondCode (Day02.makeInstructions "ULL\nRRDDD\nLURDL\nUUUUD") @?= "5DB3"
  ]
