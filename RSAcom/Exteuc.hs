-- Exteuc.hs
-- An implementation of the extended Euclidean algorithm

module Exteuc where

import Data.Maybe

--Note: This function will only return the gcd of the two numbers, and not the
-- a/b values typically returned by extended Euclidean algorithms.
exteuc :: Integer -> Integer -> (Integer, Integer, Integer)
exteuc a 0 = (1, 0, a)
exteuc a b = (t, x - q * t, g) where
    (q, r) = a `quotRem` b
    (x, t, g) = exteuc b r
