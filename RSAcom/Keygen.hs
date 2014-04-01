-- Keygen: A module that will generate an RSA public/private keypair, given two
-- prime numbers as input.

module Keygen where

import Exteuc --Our extended Euclidean algorithm implementation

--Import System.IO to be able to get the arguments
import System.Environment
--Import Data.List to get the overloaded version of !!
import Data.List

--Given two prime numbers, outputs (Modulo, PublicKey, PrivateKey)
genkeys :: Integer -> Integer -> (Integer, Integer, Integer)
genkeys p q = (n, e, d) where
    n = p * q
    t = (p - 1) * (q - 1)
    e = getcoprime t 0
    d = modmultinv e t

--This function will generate a number coprime to the one provided.
getcoprime :: Integer -> Integer -> Integer
getcoprime t test
    --A number coprime to t is such that gcd(t, e) = 1; The gcd function in
    -- prelude is very efficient, so we simply must pick numbers of
    -- the appropriate size until one that passes the gcd test is found
    --Following common convention, the default value will be 65537, which will
    -- be incremented if it just so happens that it isn't coprime to the number.
    | test == 0 = getcoprime t 65537
    | gcd test t /= 1 = getcoprime t (test + 2)
    | otherwise = test

--This outputs the modular multiplicative inverse of a number e relative to a
-- modulus t using the implementation of the extended euclidean algoritm exteuc
-- imported in the module Exteuc
modmultinv e t
    | i < 0 = i + t
    | otherwise = i where
        (i, _, _) = exteuc e t
