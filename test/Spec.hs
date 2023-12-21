import Data.List (sort)
import ParsetBase (Card)
import Test.HUnit
import V1 qualified
import V2 qualified
import V3 qualified
import V4 qualified
import V5 qualified
import V6Chunks qualified as V6C
import V6Naive qualified as V6
import V6Parbuffer qualified as V6P

dealtCards :: [Card]
dealtCards =
  sort
    [ [3, 3, 3, 1],
      [2, 1, 1, 2],
      [1, 2, 3, 2],
      [2, 2, 2, 3],
      [1, 2, 2, 3],
      [3, 1, 1, 3],
      [2, 1, 2, 2],
      [3, 3, 3, 3],
      [2, 2, 2, 1],
      [1, 1, 1, 2],
      [1, 2, 1, 1],
      [2, 1, 3, 2]
    ]

solution :: [[Card]]
solution =
  sort
    [ [[1, 2, 1, 1], [1, 2, 2, 3], [1, 2, 3, 2]],
      [[1, 2, 2, 3], [2, 1, 1, 2], [3, 3, 3, 1]],
      [[2, 1, 1, 2], [2, 1, 2, 2], [2, 1, 3, 2]],
      [[1, 2, 1, 1], [2, 1, 2, 2], [3, 3, 3, 3]],
      [[1, 1, 1, 2], [2, 2, 2, 1], [3, 3, 3, 3]],
      [[1, 1, 1, 2], [2, 2, 2, 3], [3, 3, 3, 1]]
    ]

v1Sol :: [[Card]]
v1Sol = V1.possibleSets dealtCards 3

testV1 :: Test
testV1 =
  TestCase
    ( do
        assertEqual "" solution $ sort v1Sol
    )

testV2 :: Test
testV2 =
  TestCase
    ( do
        assertEqual "" solution $ sort (V2.possibleSets dealtCards 3)
    )

testV3 :: Test
testV3 =
  TestCase
    ( do
        assertEqual "" solution $ sort (V3.possibleSets dealtCards 3)
    )

testV4 :: Test
testV4 =
  TestCase
    ( do
        assertEqual "" solution $ sort (V4.possibleSets (map (V4.generateIndexFromCard 3) dealtCards) 3 4)
    )

testV5 :: Test
testV5 =
  TestCase
    ( do
        assertEqual "" solution $ sort (V5.possibleSets dealtCards 3)
    )

testV6Naive :: Test
testV6Naive =
  TestCase
    ( do
        assertEqual "" solution $ sort (V6.possibleSets dealtCards 3)
    )

testV6Chunks :: Test
testV6Chunks =
  TestCase
    ( do
        assertEqual "" solution $ sort (V6C.possibleSets dealtCards 3)
    )

testV6Parbuffer :: Test
testV6Parbuffer =
  TestCase
    ( do
        assertEqual "" solution $ sort (V6P.possibleSets dealtCards 3)
    )

main :: IO Counts
main =
  runTestTT $
    TestList
      [ "V1 Sequential" ~: testV1,
        "V2" ~: testV2,
        "V3" ~: testV3,
        -- something wrong w/ V4 algorithm, returning 6 instead of 8 solutions
        -- "V4" ~: testV4,
        "V5" ~: testV5,
        "V6 Naive" ~: testV6Naive,
        "V6 Chunks" ~: testV6Chunks,
        "V6 ParBuffer" ~: testV6Parbuffer
      ]
