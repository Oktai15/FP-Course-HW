module TestFile where

import qualified AdtWorld           as Aw
import           Data.List          (findIndex)
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Maybe         (fromJust, isNothing)
import qualified Foldable           as Fl


-- Tests for SimpleFunctions module (if tests consist of pair args then use uncurry before function)
order3Tests :: [(Double, Double, Double)]
order3Tests = [(-2.0, 4.5, 1.1), (18.0, 4.5, 500.002)]

order3Ans :: [(Double, Double, Double)]
order3Ans = [(-2.0, 1.1, 4.5), (4.5, 18.0,  500.002)]

smartReplicateTests :: [[Int]]
smartReplicateTests = [[1, 2, 3], [4, 1, 1], [5, 2, 8], [1, 4, 2], [6, 2, 6]]

smartReplicateAns :: [[Int]]
smartReplicateAns = [ [1, 2, 2, 3, 3, 3]
                    , [4, 4, 4, 4, 1, 1]
                    , [5, 5, 5, 5, 5, 2, 2, 8, 8, 8, 8, 8, 8, 8, 8]
                    , [1, 4, 4, 4, 4, 2, 2]
                    , [6, 6, 6, 6, 6, 6, 2, 2, 6, 6, 6, 6, 6, 6]
                    ]

-- Example: doTest containsTests containsAns (uncurry contains)
containsTests :: [(Char, [String])]
containsTests = [ ('b', [['z', 'b', 'a', 'q'], [], ['a', 'x'], ['b']])
                , ('x', [['z', 'b', 'a', 'q'], [], ['b', 'x'], ['b']])
                ]

containsAns :: [[String]]
containsAns  = [ [['z', 'b', 'a', 'q'], ['b']]
               , [['b', 'x']]
               ]

stringSumTests :: [String]
stringSumTests = [ "1", "1 2 3", " 1", "1 ", "\t1\t", "\t12345\t", "010 020 030"
                 , " 123 456 789 ", "-1", "-1 -2 -3", "\t-12345\t", " -123 -456 -789 "
                 , "\n1\t\n3   555  -1\n\n\n-5", "123\t\n\t\n\t\n321 -4 -40"
                 ]

stringSumAns :: [Int]
stringSumAns = [1, 6, 1, 1, 1, 12345, 60, 1368, -1, -6, -12345, -1368, 553, 400]

-- Tests for PatternMatching module
removeByIndexTests :: [(Int, [Int])]
removeByIndexTests = [(1, [1, 2, 3]), (4, [1, 2, 3]), (1, [])]

removeByIndexAns :: [([Int], Maybe Int)]
removeByIndexAns = [([1, 3], Just 2), ([1, 2, 3], Nothing), ([], Nothing)]

mergeSortTests :: [[Int]]
mergeSortTests = [[1, 2, 3, 4, 5, 2], [4, 2, 1, 4], [7, 2, 2, 2, 1, 3, 3], [10, 12, 3, 4]]

mergeSortAns :: [[Int]]
mergeSortAns = [[1, 2, 2, 3, 4, 5], [1, 2, 4, 4], [1, 2, 2, 2, 3, 3, 7], [3, 4, 10, 12]]

-- Tests for AdtWorld module
daysToPartyTests :: [Aw.DayOfWeek]
daysToPartyTests = [Aw.Mn, Aw.Fr, Aw.St]

daysToPartyAns :: [Int]
daysToPartyAns = [4, 0, 6]

housesLess10 :: [Aw.Houses]
housesLess10 = [Aw.OneHouse Aw.Two, Aw.ManyHouses Aw.Three (Aw.ManyHouses Aw.Three (Aw.OneHouse Aw.Three))]

-- housesMore10 :: [Aw.Houses]
-- housesMore10 = [ Aw.ManyHouses Aw.Three (Aw.ManyHouses Aw.Three (Aw.OneHouse Aw.Four))
--                , Aw.ManyHouses Aw.Four (Aw.ManyHouses Aw.Three (Aw.OneHouse Aw.Three))
--                ]

villages :: [Aw.Village]
villages = [ Aw.Village { Aw.mbCastle = Nothing,
                         Aw.mbLibraryOrChurch = Nothing,
                         Aw.houses = head housesLess10 }
           , Aw.Village { Aw.mbCastle = Just Aw.Castle{ Aw.isLord = True, Aw.isWalls = False },
                          Aw.mbLibraryOrChurch = Nothing,
                          Aw.houses = housesLess10 !! 1}
           ]
buildFirstCastleTests :: [Aw.Village]
buildFirstCastleTests = [ head villages, villages !! 1]

buildFirstCastleAns :: [Either Aw.Village Aw.Village]
buildFirstCastleAns = [Right ((head villages) {
    Aw.mbCastle = Just Aw.Castle{ Aw.isLord = False, Aw.isWalls = False }}), Left (villages !! 1)]

natDivTests :: [(Aw.Nat, Aw.Nat)]
natDivTests = [(10, 3),
               (11, 3),
               (12, 3),
               (12, 0)]
natDivAns :: [Either String Aw.Nat]
natDivAns = [ Right 3
            , Right 3
            , Right 4
            , Left "Division by zero"
            ]

natModTests :: [(Aw.Nat, Aw.Nat)]
natModTests = [(10, 3),
               (11, 3),
               (12, 3),
               (12, 0)]
natModAns :: [Either String Aw.Nat]
natModAns = [ Right 1
            , Right 2
            , Right 0
            , Left "Division by zero"
            ]

deleteElemTests :: [(Int, Aw.Tree Int)]
deleteElemTests = [ (1, Aw.Node (5 :| [5,5]) (Aw.Node (1 :| [])
                    Aw.Leaf (Aw.Node (2 :| [2]) Aw.Leaf Aw.Leaf))
                    (Aw.Node (9 :| []) (Aw.Node (7 :| []) Aw.Leaf Aw.Leaf) Aw.Leaf))
                  , (5, Aw.Node (3 :| [3]) (Aw.Node (2 :| [2])
                    (Aw.Node (1 :| []) Aw.Leaf Aw.Leaf) Aw.Leaf)
                    (Aw.Node (5 :| []) Aw.Leaf (Aw.Node (8 :| []) Aw.Leaf Aw.Leaf)))
                  ]

deleteElemAns :: [Aw.Tree Int]
deleteElemAns = [ Aw.Node (5 :| [5,5]) (Aw.Node (2 :| [2]) Aw.Leaf Aw.Leaf)
                  (Aw.Node (9 :| []) (Aw.Node (7 :| []) Aw.Leaf Aw.Leaf) Aw.Leaf)
                , Aw.Node (3 :| [3]) (Aw.Node (2 :| [2]) (Aw.Node (1 :| []) Aw.Leaf Aw.Leaf) Aw.Leaf)
                  (Aw.Node (8 :| []) Aw.Leaf Aw.Leaf)
                ]
-- Note: import Data.Foldable (toList)
toListTests :: [Aw.Tree Int]
toListTests = map Aw.fromList mergeSortTests

toListAns :: [[Int]]
toListAns = mergeSortAns

-- Tests for Foldable module
splitOnTests :: [(Char, String)]
splitOnTests = [('/', "my/path/to/file"), ('&', "my&abc&defg&"), ('/', "")]

splitOnAns :: [Fl.NonEmpty String]
splitOnAns = [ "my" Fl.:| ["path","to","file"]
             , "my" Fl.:| ["abc","defg",""]
             , ""   Fl.:| []
             ]

joinWithTests :: [(Char, Fl.NonEmpty String)]
joinWithTests = [ ('/', "my" Fl.:| ["path", "to", "file"])
                , ('&', "my" Fl.:| ["abc", "defg", ""])
                , ('/', ""   Fl.:| [])
                ]

joinWithAns :: [String]
joinWithAns = ["my/path/to/file", "my&abc&defg&", ""]

doTest :: (Show t, Show a, Eq a) => [t] -> [a] -> (t -> a) -> String
doTest tests ans f = if isNothing numFail
                     then "All tests passed!"
                     else let ind = fromJust numFail
                          in "Tests failed! " ++ "Fail test: " ++
                           show (tests !! ind) ++ "; " ++ "Right answer: " ++
                           show (ans !! ind) ++ "; " ++ "Your answer: " ++ show (myAns !! ind)
                       where
                         myAns   = map f tests
                         numFail = findIndex (uncurry (/=)) (zip ans myAns)

