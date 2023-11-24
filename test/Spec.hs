import qualified System.Exit as Exit
import Test.HUnit
import Circuits (halfAdder)


testHalfAdder :: Test
testHalfAdder = TestCase (assertBool "fail" result) where
    result :: Bool
    result = all (\pair -> halfAdder pair == halfAdderTable pair) [(x, y) | x <- [False, True], y <- [False, True]]
    halfAdderTable (x, y) = case (x, y) of
        (False, False) -> (False, False)
        (False, True) -> (True, False)
        (True, False) -> (True, False)
        (True, True) -> (False, True)

tests :: Test
tests = TestList [TestLabel "halfAdder" testHalfAdder]
main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess

