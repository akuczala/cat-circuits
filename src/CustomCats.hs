{-# LANGUAGE DataKinds #-}
module CustomCats(
    Bimap(..),
    Closed(..)
) where
import Control.Category

class Category k => Bimap k where
    bimap :: k a b -> k a c -> k (a, a) (b, c)

instance Bimap (->) where
    bimap f g (x,y) = (f x, g y)

class Category k => Closed k where
    apply :: k (a -> b, a) b
    curry :: k (a, b) c -> k a (b -> c)
    uncurry :: k a (b -> c) -> k (a, b) c

instance Closed (->) where
    apply (f, x) = f x
    curry = Prelude.curry
    uncurry = Prelude.uncurry