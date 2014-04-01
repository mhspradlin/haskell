-- expsq.hs
-- An implementation of exponentiation by squares.

module Expsq where

expsq :: Integer -> Integer -> Integer -> Integer
expsq base exponent modulo
    | exponent == 0 = 1
    | exponent == 1 = mod base modulo
    | mod exponent 2 == 0 = expsq (mod (base * base) modulo) (exponent `quot` 2)
							     modulo
    | mod exponent 2 == 1 = mod ((mod base modulo) * expsq (mod (base * base) 
									modulo)
					   ((exponent - 1) `quot` 2)
					   modulo) modulo
