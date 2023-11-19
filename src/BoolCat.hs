module BoolCat(
  BoolCat(..)
) where
import Control.Category.Cartesian
import Graph

class Cartesian k => BoolCat k where
  notC :: k Bool Bool
  andC, orC, xorC :: k (Bool, Bool) Bool

instance BoolCat (->) where
  notC = not
  andC = uncurry (&&)
  orC = uncurry (||)
  xorC (x, y) = x /= y

instance BoolCat Graph where
  notC = genNode "not"
  andC = genNode "and"
  orC = genNode "or"
  xorC = genNode "xor"