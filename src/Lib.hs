
module Lib
    ( someFunc
    ) where
import Control.Category ((>>>))
import Control.Category.Cartesian (Cartesian (copy))
import Control.Category.Monoidal (first')
import Control.Monad.ST (ST)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

class MyPrimitives k where
    reverseString :: k String String
    eq :: Eq a => k (a, a) Bool

instance MyPrimitives (->) where
    reverseString = reverse
    eq (x, y) = x == y

-- isPalendrome :: String -> Bool
-- isPalendrome = (\s -> (s, s))
--     >>> (\(s, s') -> (reverse s, s'))
--     >>> (\(revS, s) -> revS == s)

-- import Data.Bifunctor (first)
-- isPalendrome' :: String -> Bool
-- isPalendrome' = (\s -> (s, s))
--     >>> first reverse
--     >>> uncurry (==)

isPalendrome ::
    MyPrimitives k
    => Cartesian k
    => k String Bool
isPalendrome =
    copy
    >>> first' reverseString
    >>> eq

palTest :: String -> Bool
palTest = isPalendrome

