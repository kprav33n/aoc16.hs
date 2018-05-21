{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit

import Day01
import Day02
import Day03
import Day04
import Day05

main :: IO ()
main = defaultMain tests
tests :: TestTree
tests = testGroup "Tests" [day01Tests, day02Tests, day03Tests, day04Tests, day05Tests]

day01Tests :: TestTree
day01Tests = testGroup "Day01 tests" [taxicabDistanceTests, makeInstructionsTests, hqDistanceTests]

taxicabDistanceTests :: TestTree
taxicabDistanceTests = testGroup "taxicabDistance"
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
makeInstructionsTests = testGroup "makeInstructions"
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
day02Tests = testGroup "Day02 tests"
  [ testCase "bathroomCode" $
    bathroomCode (Day02.makeInstructions "ULL\nRRDDD\nLURDL\nUUUUD") @?= "1985"

  , testCase "bathroomDiamondCode" $
    bathroomDiamondCode (Day02.makeInstructions "ULL\nRRDDD\nLURDL\nUUUUD") @?= "5DB3"
  ]

day03Tests :: TestTree
day03Tests = testGroup "Day 03 tests"
  [
    testCase "numPossibleTriangles" $
    numPossibleTriangles (Day03.parseInput "5  10  25\n 5   7  9") @?= 1

  , testCase "numPossibleTriangles" $
    numPossibleTriangles2 (Day03.parseInput "5  6  11\n 10   7  2\n25 6 20") @?= 1
  ]

day04Tests :: TestTree
day04Tests = testGroup "Day 04 tests"
  [ testCase "sectorSum" $
    sectorSum (Day04.parseInput "aaaaa-bbb-z-y-x-123[abxyz]\n\
                                \a-b-c-d-e-f-g-h-987[abcde]\n\
                                \not-a-real-room-404[oarel]\n\
                                \totally-real-room-200[decoy]") @?= 1514
  , testCase "decryptEntry" $
    decryptEntry ("qzmt-zixmtkozy-ivhz",343,"xxx") @?= ("very encrypted name",343,"xxx")

  , testCase "northPoleSector" $
    northPoleSector (Day04.parseInput "ghkmaihex-hucxvm-lmhktzx-267[hmxka]\n\
                                      \ftzgxmbv-vtgwr-wxlbzg-267[gbtvw]\n\
                                      \ubhatstkwhnl-xzz-kxvxbobgz-267[umogq]") @?= 267
  ]

day05Tests :: TestTree
day05Tests = testGroup "Day 05 tests"
  [ testCase "doorPassword" $
    doorPassword 2 "abc" @?= "18"
  ]
