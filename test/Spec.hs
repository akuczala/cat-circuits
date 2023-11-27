{-# LANGUAGE DataKinds #-}
import qualified System.Exit as Exit
import Test.HUnit
import qualified Data.Vector.Sized as V
import Circuits
import Control.Category ((>>>))
import Control.Category.Monoidal (MonoidalProduct(..))
import GHC.TypeNats
import Data.Data (Proxy (..))


testTrue :: Bool -> Test
testTrue result = TestCase (assertBool "fail" result)

funEqTable :: Eq b => (a -> b) -> [(a, b)] -> Bool
funEqTable f = all (\(input, output) -> f input == output)

testBoolVecToIntBigEnd :: Test
testBoolVecToIntBigEnd = testTrue $ funEqTable boolVecToIntBigEnd table1 && funEqTable boolVecToIntBigEnd table2 where
    table1 = [
        (V.fromTuple (True, False, False), 4),
        (V.fromTuple (False, True, True), 3)
        ]
    table2 = [
        (V.fromTuple (True, False, True, False, True), 21),
        (V.fromTuple (False, True, True, False, False), 12)
        ]

testBoolVecToIntLilEnd :: Test
testBoolVecToIntLilEnd = testTrue $ funEqTable boolVecToIntLilEnd table1 && funEqTable boolVecToIntLilEnd table2 where
    table1 = [
        (V.fromTuple (True, False, False), 1),
        (V.fromTuple (False, True, True), 6)
        ]
    table2 = [
        (V.fromTuple (True, False, True, False, True), 21),
        (V.fromTuple (False, True, True, False, False), 6)
        ]

testIntToBigEnd :: Test
testIntToBigEnd = testTrue $ funEqTable intToBoolVecBigEnd table1 && funEqTable intToBoolVecBigEnd table2  where
    table1 = [
        (5, V.fromTuple (True, False, True)),
        (9, V.fromTuple (False, False, True))
        ]
    table2 = [
        (11, V.fromTuple (True, False, True, True)),
        (7, V.fromTuple (False, True, True, True))
        ]

testIntToLilEnd :: Test
testIntToLilEnd = testTrue $ funEqTable intToBoolVecLilEnd table1 && funEqTable intToBoolVecLilEnd table2  where
    table1 = [
        (5, V.fromTuple (True, False, True)),
        (3, V.fromTuple (True, True, False))
        ]
    table2 = [
        (12, V.fromTuple (False, False, True, True)),
        (20, V.fromTuple (False, False, True, False))
        ]


testHalfAdder :: Test
testHalfAdder = testTrue result where
    result :: Bool
    result = all (\pair -> halfAdder pair == halfAdderTable pair) [(x, y) | x <- [False, True], y <- [False, True]]
    halfAdderTable (x, y) = case (x, y) of
        (False, False) -> (False, False)
        (False, True) -> (True, False)
        (True, False) -> (True, False)
        (True, True) -> (False, True)

testFullAdder :: Test
testFullAdder = testTrue result where
    result :: Bool
    result = all (\input -> fullAdder halfAdder input == fullAdderTable input) [(cin, (x, y)) | cin <- [False, True], x <- [False, True], y <- [False, True]]
    fullAdderTable (cin, (x, y)) = case (cin, x, y) of
        -- (cin, x, y) -> (sum, cout)
        (False, False, False) -> (False, False)
        (False, False, True) -> (True, False)
        (False, True, False) -> (True, False)
        (False, True, True) -> (False, True)
        (True, False, False) -> (True, False)
        (True, False, True) -> (False, True)
        (True, True, False) -> (False, True)
        (True, True, True) -> (True, True)
    
testTwoBitAdder :: Test
testTwoBitAdder = testTrue $ funEqTable testFun table where
    testFun :: (Bool, (Int, Int)) -> (Int, Bool)
    testFun =
        second' (intToBoolVecLilEnd *** intToBoolVecLilEnd)
        >>> twoBitAdder (fullAdder halfAdder)
        >>> first' boolVecToIntLilEnd
    table = [
        (
            (cin, (x, y)),
            (
                mod (x + y + fromEnum cin) 4 ,
                (x + y + fromEnum cin) `div` 4 == 1
            )
        )
            | cin <- [False, True], x <- [0, 1, 2], y <- [0, 1, 2]
        ]

testNBitAdder :: (KnownNat n) => Proxy n -> Test
testNBitAdder proxN = testTrue $ funEqTable testFun table where
    testFun :: (Bool, (Int, Int)) -> Int
    testFun =
        second' (intToBoolVecLilEnd *** intToBoolVecLilEnd)
        >>> adder proxN
        >>> boolVecToIntLilEnd
    adder :: Proxy n -> (Bool, (V.Vector n Bool, V.Vector n Bool)) -> V.Vector n Bool
    adder _ = nBitAdder (fullAdder halfAdder)
    intN :: Int
    intN = (fromIntegral . toInteger . natVal) proxN
    powIntN = 2 ^ intN
    table = [
        (
            (cin, (x, y)),
            (
                mod (x + y + fromEnum cin) powIntN
            )
        )
            | cin <- [False, True], x <- [0 .. powIntN - 1], y <- [0 .. powIntN - 1]
        ]

tests :: Test
tests = TestList [
    TestLabel "boolVecToIntBigEnd" testBoolVecToIntBigEnd,
    TestLabel "boolVecToIntLilEnd" testBoolVecToIntLilEnd,
    TestLabel "testIntToBigEnd" testIntToBigEnd,
    TestLabel "testIntToLilEnd" testIntToLilEnd,
    TestLabel "halfAdder" testHalfAdder,
    TestLabel "fullAdder" testFullAdder,
    TestLabel "twoBitAdder" testTwoBitAdder,
    TestLabel "nBitAdder" $ testNBitAdder (Proxy :: Proxy 4)
    ]
main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess

