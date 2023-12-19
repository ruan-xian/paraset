import Data.List (sort)
import Lib (possibleSets)
import Test.HUnit

testSeq1 :: Test
testSeq1 =
  TestCase
    ( do
        let dealtCards =
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
            solution =
              [ [[1, 2, 1, 1], [1, 2, 2, 3], [1, 2, 3, 2]],
                [[1, 2, 2, 3], [2, 1, 1, 2], [3, 3, 3, 1]],
                [[2, 1, 1, 2], [2, 1, 2, 2], [2, 1, 3, 2]],
                [[1, 2, 1, 1], [2, 1, 2, 2], [3, 3, 3, 3]],
                [[1, 1, 1, 2], [2, 2, 2, 1], [3, 3, 3, 3]],
                [[1, 1, 1, 2], [2, 2, 2, 3], [3, 3, 3, 1]]
              ]
        assertEqual "" solution $ possibleSets dealtCards 3
    )

main :: IO Counts
main =
  runTestTT $
    TestList
      ["Sequential (with 12 3 4)" ~: testSeq1]
