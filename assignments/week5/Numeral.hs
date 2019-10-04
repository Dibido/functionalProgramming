module Numeral where

import GHC.Float hiding ((**))
import Prelude hiding ((**))

infixl **
a ** b = (2 * a) + b

getBaseConverter :: Integer -> (Integer -> Integer -> Integer)
getBaseConverter base = (\x y -> (base * x) + y)

type Base   =  Integer
type Digit  =  Integer

msdf, lsdf :: Base -> [Digit] -> Integer

msdf b l = foldl (\x r -> getBaseConverter b x r) 0 l

lsdf b l = msdf b (reverse l) 
