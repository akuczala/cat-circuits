{-# LANGUAGE DataKinds #-}
module CustomCats(
    Bimap(..),
) where
import Control.Category
import Control.Category.Cartesian

class Category k => Bimap k where
    bimap :: k a b -> k a c -> k (a, a) (b, c)

instance Bimap (->) where
    bimap f g (x,y) = (f x, g y)