module ConalCats(
    Cartesian(..),
    MonoidalCat(..),
) where
import Control.Category
import qualified Control.Category as C

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

class Category k => MonoidalCat k where
    bimap :: k a b -> k a c -> k (a, a) (b, c)

instance MonoidalCat (->) where
    bimap f g (x,y) = (f x, g y)