module BoolCat(
  BoolCat(..)
) where
import Control.Category.Cartesian
import Graph
import Control.Category ((>>>))

class Cartesian k => BoolCat k where
  notC :: k Bool Bool
  andC, orC, xorC, nandC, norC :: k (Bool, Bool) Bool
  true :: k a Bool
  false :: k a Bool

instance BoolCat (->) where
  notC = not
  andC = uncurry (&&)
  orC = uncurry (||)
  xorC (x, y) = x /= y
  nandC = not . andC
  norC = not . orC
  true = const True
  false = const False

instance BoolCat Graph where
  notC = genNodeFn "not" notC
  andC = genNodeFn "and" andC
  orC = genNodeFn "or" orC
  xorC = genNodeFn "xor" xorC
  nandC = genNodeFn "nand" nandC
  norC = genNodeFn "nor" norC
  true = consume >>> genNodeFn "true" (const True)
  false = consume >>> genNodeFn "false" (const False)
  
-- using NAND primitives
-- instance BoolCat Graph where
--   nandC = genNode "NAND"
--   notC = copy >>> nandC
--   andC = nandC >>> notC
--   orC = notC *** notC >>> nandC
--   xorC = copy >>> (first' notC >>> andC) *** (second' notC >>> andC) >>> orC