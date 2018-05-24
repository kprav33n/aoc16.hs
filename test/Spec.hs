{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit

import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import Day09

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ day01Tests
  , day02Tests
  , day03Tests
  , day04Tests
  , day05Tests
  , day06Tests
  , day07Tests
  , day08Tests
  , day09Tests
  ]

day01Tests :: TestTree
day01Tests = testGroup "Day 01" [taxicabDistanceTests, makeInstructionsTests, hqDistanceTests]

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
hqDistanceTests = testGroup "hqDistance"
  [ testCase "R8, R4, R4, R8" $
    hqDistance [R 8, R 4, R 4, R 8] @?= 4
  ]

day02Tests :: TestTree
day02Tests = testGroup "Day 02"
  [ testCase "bathroomCode" $
    bathroomCode (Day02.makeInstructions "ULL\nRRDDD\nLURDL\nUUUUD") @?= "1985"

  , testCase "bathroomDiamondCode" $
    bathroomDiamondCode (Day02.makeInstructions "ULL\nRRDDD\nLURDL\nUUUUD") @?= "5DB3"
  ]

day03Tests :: TestTree
day03Tests = testGroup "Day 03"
  [
    testCase "numPossibleTriangles" $
    numPossibleTriangles (Day03.parseInput "5  10  25\n 5   7  9") @?= 1

  , testCase "numPossibleTriangles" $
    numPossibleTriangles2 (Day03.parseInput "5  6  11\n 10   7  2\n25 6 20") @?= 1
  ]

day04Tests :: TestTree
day04Tests = testGroup "Day 04"
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
day05Tests = testGroup "Day 05"
  [ testCase "doorPassword" $
    doorPassword 2 "abc" @?= "18"

  , testCase "doorPassword2" $
    doorPassword2 2 "abc" @?= "05"
  ]

day06Tests :: TestTree
day06Tests = testGroup "Day 06"
  [ testCase "correctMessage - most frequent" $
    correctMessage last "eedadn\n\
                         \drvtee\n\
                         \eandsr\n\
                         \raavrd\n\
                         \atevrs\n\
                         \tsrnev\n\
                         \sdttsa\n\
                         \rasrtv\n\
                         \nssdts\n\
                         \ntnada\n\
                         \svetve\n\
                         \tesnvt\n\
                         \vntsnd\n\
                         \vrdear\n\
                         \dvrsen\n\
                         \enarar\n" @?= "easter"
  , testCase "correctMessage - least frequent" $
    correctMessage head "eedadn\n\
                         \drvtee\n\
                         \eandsr\n\
                         \raavrd\n\
                         \atevrs\n\
                         \tsrnev\n\
                         \sdttsa\n\
                         \rasrtv\n\
                         \nssdts\n\
                         \ntnada\n\
                         \svetve\n\
                         \tesnvt\n\
                         \vntsnd\n\
                         \vrdear\n\
                         \dvrsen\n\
                         \enarar\n" @?= "advent"
  ]

day07Tests :: TestTree
day07Tests =
  testGroup
    "Day 07"
    [ testCase "countTLSSupportedIPs" $
      countTLSSupportedIPs
        "abba[mnop]qrsnt\n\
        \abcd[bddb]xyyx\n\
        \aaaa[qwer]tyui\n\
        \ioxxoj[asdfgh]zxcvbn\n\
        \abba[bddb]qrsnt\n\
        \abbabccb\n" @?=
      3
    , testCase "countSSLSupportedIPs" $
      countSSLSupportedIPs
        "aba[bab]xyz\n\
        \xyx[xyx]xyx\n\
        \aaa[kek]eke\n\
        \zazbz[bzb]cdb" @?=
      3
    ]

day08Tests :: TestTree
day08Tests =
  testGroup
    "Day 08"
    [ testCase "numLitPixelsAfter" $
      (numLitPixelsAfter 7 3 . Day08.parseInput)
        "rect 3x2\n\
        \rotate column x=1 by 1\n\
        \rotate row y=0 by 4\n\
        \rotate column x=1 by 1" @?= 6
    ]

day09Tests :: TestTree
day09Tests =
  testGroup
    "Day 09"
    [ testCase "decompress" $ do
        decompressedLength "ADVENT" @?= 6
        decompressedLength "A(1x5)BC" @?= 7
        decompressedLength "(3x3)XYZ" @?= 9
        decompressedLength "A(2x2)BCD(2x2)EFG" @?= 11
        decompressedLength "(6x1)(1x3)A" @?= 6
        decompressedLength "X(8x2)(3x3)ABCY" @?= 18

    , testCase "decompressedLength2" $ do
        decompressedLength2 "(3x3)XYZ" @?= 9
        decompressedLength2 "X(8x2)(3x3)ABCY" @?= 20
        decompressedLength2 "(27x12)(20x12)(13x14)(7x10)(1x12)A" @?= 241920
        decompressedLength2 "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN" @?= 445
    ]
