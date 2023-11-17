{-# LANGUAGE GADTs #-}
module Serial where
import Control.Category (Category (id, (.)))
import Control.Category.Monoidal (MonoidalProduct (first', second'), SymmetricProduct (swap))

data FreeFunc p a b where
    Id :: FreeFunc p x x
    Compose :: FreeFunc p x y -> FreeFunc p y z-> FreeFunc p x z
    Copy :: FreeFunc p a (a,a)
    First :: FreeFunc p a b -> FreeFunc p (a, x) (b, x)
    Second :: FreeFunc p a b -> FreeFunc p (x, a) (x, b)
    Fst :: FreeFunc p (a, x) a
    Snd :: FreeFunc p (x, a) a

instance Category (FreeFunc p ) where
    id = Id
    f . g = Compose g f

-- instance MonoidalProduct (FreeFunc p ) where
--   first' = _
--   second' = _
    
-- instance SymmetricProduct (FreeFunc p) where
--     swap (FreeFunc p x y) = FreeFunc p y x
