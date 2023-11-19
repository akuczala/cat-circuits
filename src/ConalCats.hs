module ConalCats(
    Cartesian(..),
    Bimap(..),
    SymmetricProduct(..),
    MonoidalProduct(..)
) where
import Control.Category
import qualified Control.Category as C
import qualified Control.Arrow as A

-- infixr 9 .
-- class Category k where
--   id :: k a a
--   (.) :: k b c -> k a b -> k a c

-- instance Category (->) where
--   id = Prelude.id
--   g . f = g Prelude.. f

infixr 3 &&&
class Category k => Cartesian k where
  (&&&) :: k a c -> k a d -> k a (c, d)
  fst' :: k (a, b) a
  snd' :: k (a, b) b
  -- add these from Penner
  consume :: k a ()
  copy :: k a (a, a)
  copy = C.id &&& C.id

instance Cartesian (->) where
  f &&& g = \x -> (f x, g x)
  fst' = fst
  snd' = snd
  consume _ = ()
  --copy x = (x, x)

-- thing :: Cartesian k => k a c -> k a d -> k a (c, d)
-- thing f g = _ f g C.. copy

class Category k => Bimap k where
    bimap :: k a b -> k a c -> k (a, a) (b, c)

instance Bimap (->) where
    bimap f g (x,y) = (f x, g y)

class Category k => SymmetricProduct k where
  swap :: k (l, r) (r, l)
  reassoc :: k (a, (b, c)) ((a, b), c)

instance SymmetricProduct (->) where
  swap (x, y) = (y, x)
  reassoc (x, (y, z)) = ((x, y), z)

class SymmetricProduct k => MonoidalProduct k where
  {-# MINIMAL first' | second' #-}
  (***) :: k a1 b1 -> k a2 b2 -> k (a1, a2) (b1, b2)
  f1 *** f2 = first' f1 >>> second' f2
  first' :: k a b -> k (a, c) (b, c)
  first' f = swap >>> second' f >>> swap
  second' :: k a b -> k (c, a) (c, b)
  second' f = swap >>> first' f >>> swap
  
instance MonoidalProduct (->) where
  (***) = (A.***)
  first' = A.first
  second' = A.second