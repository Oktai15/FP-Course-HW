module TestFile2 where

import           Data.Maybe          (isJust, isNothing)

import           Hedgehog            (Gen, Property, assert, forAll, property)
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range
import           Test.Tasty          (TestTree)
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.Hspec    (Spec, describe, it, shouldBe,
                                      shouldSatisfy, testSpec)

import           HierarchyOfTypes    (stringSum)
import           MonadicCalc         (ArithmeticError (..), Expr (..), bin,
                                      eval)
import           ParserCombinators   (Parser (..), parseBrackets, parseInt,
                                      parseLists)

--Block 1
evalTest1 :: Expr
evalTest1 = Sub (Add (Const 8) (Const 4)) (Const 2)

evalTest2 :: Expr
evalTest2 = Pow (Mul (Div (Const 10) (Const 2)) (Const 3)) (Const 2)

evalTest3 :: Expr
evalTest3 = Div (Const 5) (Const 0)

evalTest4:: Expr
evalTest4 = Pow (Const 5) (Const (-2))

specEvalTree :: IO TestTree
specEvalTree = testSpec "expr: unit" specEval

specEval :: Spec
specEval =
  describe "simple tests" $ do
    it "test 1: sub, add, const" $
      eval evalTest1 `shouldBe` Right 10
    it "test 2: mul, div, pow" $
      eval evalTest2 `shouldBe` Right 225
    it "test 3: bad div" $
      eval evalTest3 `shouldBe` Left DivisionByZero
    it "test 4: bad pow" $
      eval evalTest4 `shouldBe` Left NegativeExponentiation

genBin :: Gen Int -> Gen (Int, [Int])
genBin randomLength = randomLength >>= \len -> randomList len
                                   >>= \list -> return (len, list)
  where
    randomList len = Gen.list (Range.singleton len) rangeValues
    rangeValues = Gen.integral (Range.constantFrom 0 0 1)

propertyBinTree :: TestTree
propertyBinTree = testProperty "bin: property" propertyBin

propertyBin :: Property
propertyBin = property $ forAll (genBin randomLength) >>=
              \(len, b) -> assert $ b `elem` bin len
  where
    randomLength = Gen.integral rangeLength
    rangeLength = Range.constantFrom 0 0 10

--Block 2
specStringSumTree :: IO TestTree
specStringSumTree = testSpec "stringSum: unit" specStringSum

specStringSum :: Spec
specStringSum =
  describe "simple tests" $ do
    it "test 1: simple number" $
      stringSum "1" `shouldBe` Just 1
    it "test 1: three numbers" $
      stringSum "1 2 3" `shouldBe` Just 6
    it "test 3: difficult string" $
      stringSum "\n1\t\n3   555  -1\n\n\n-5" `shouldBe` Just 553
    it "test 4: simple bad string 1" $
      stringSum "asd" `shouldBe` Nothing
    it "test 5: difficult bad string 2" $
      stringSum "222 1okt4i1 222" `shouldBe` Nothing

--Block 3
specParseBracketsTree :: IO TestTree
specParseBracketsTree = testSpec "parseBrackets: unit" specParseBrackets

specParseBrackets :: Spec
specParseBrackets =
  describe "simple tests" $ do
    it "test 1: empty string" $
      runParser parseBrackets "" `shouldSatisfy` isJust
    it "test 2: one bracket" $
      runParser parseBrackets "(" `shouldSatisfy` isNothing
    it "test 3: good short string" $
      runParser parseBrackets "()" `shouldSatisfy` isJust
    it "test 4: bad long string" $
      runParser parseBrackets "()()(" `shouldSatisfy` isNothing
    it "test 5: good long string" $
      runParser parseBrackets "((())())" `shouldSatisfy` isJust

specParseIntTree :: IO TestTree
specParseIntTree = testSpec "parseInt: unit" specParseInt

specParseInt :: Spec
specParseInt =
  describe "simple tests" $ do
    it "test 1: empty string" $
      runParser parseInt "" `shouldBe` Nothing
    it "test 2: one number" $
      runParser parseInt "10" `shouldBe` Just (10, "")
    it "test 3: negative number" $
      runParser parseInt "-10" `shouldBe` Just (-10, "")
    it "test 4: explicit positive number and tail" $
      runParser parseInt "+10 -20 30" `shouldBe` Just (10, " -20 30")
    it "test 5: bad string" $
      runParser parseInt "--5a5" `shouldBe` Nothing

specParseListsTree :: IO TestTree
specParseListsTree = testSpec "parseLists: unit" specParseLists

specParseLists :: Spec
specParseLists =
  describe "simple tests" $ do
    it "test 1: empty string" $
      runParser parseLists "" `shouldBe` Nothing
    it "test 2: simple good string" $
      runParser parseLists "1, 1" `shouldBe` Just ([[1]],"")
    it "test 3: simple bad string" $
      runParser parseLists ",15," `shouldBe` Nothing
    it "test 4: tricky bad string" $
      runParser parseLists "0, 100500" `shouldBe` Just ([[]], ", 100500")
    it "test 5: long and difficult good string" $
      runParser parseLists "2, 1, +10   ,   3,5,  -7, 2" `shouldBe` Just ([[1,10],[5,-7,2]],"")

