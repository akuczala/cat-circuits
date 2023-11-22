module BoolCat(
  BoolCat(..)
) where
import Control.Category.Cartesian
import Graph
import Control.Category ((>>>))
import Control.Category.Monoidal ((***), MonoidalProduct (..))

class Cartesian k => BoolCat k where
  notC :: k Bool Bool
  andC, orC, xorC, nandC :: k (Bool, Bool) Bool

instance BoolCat (->) where
  notC = not
  andC = uncurry (&&)
  orC = uncurry (||)
  xorC (x, y) = x /= y
  nandC = not . andC

instance BoolCat Graph where
  notC = genNode "not"
  andC = genNode "and"
  orC = genNode "or"
  xorC = genNode "xor"
  nandC = genNode "nand"
  
-- using NAND primitives
-- instance BoolCat Graph where
--   nandC = genNode "NAND"
--   notC = copy >>> nandC
--   andC = nandC >>> notC
--   orC = notC *** notC >>> nandC
--   xorC = copy >>> (first' notC >>> andC) *** (second' notC >>> andC) >>> orC