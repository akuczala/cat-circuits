module Utils(
    Pair,
    OnlyOne(..),
    addAnother
) where

type Pair a = (a, a)

data OnlyOne a = NothingYet | OnlyOne a | TooMany
    deriving Show
addAnother :: a -> OnlyOne a -> OnlyOne a
addAnother x o = case o of
    NothingYet -> OnlyOne x
    _ -> TooMany