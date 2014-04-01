--RSAgen: A program that will generate an RSA public/private keypair, given two
-- prime numbers as input.

module RSAgen where

import Data.Maybe
import Exteuc

--Import System.IO to be able to get the arguments
import System.Environment
--Import Data.List to get the overloaded version of !!
import Data.List

genkeys :: Integer -> Integer -> ((Integer, Integer), (Integer, Integer))
genkeys p q = ((n, e), (n, d)) where
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

    --First, use the sieve of Eratosthenes to generate a list of prime numbers
    -- starting from two
    --esieve (primenum : nums) = primenum : esieve [primes | primes <- nums, 
    --                primes `mod` primenum > 0]
    
    --Since we want prime numbers that are of the order of 2^80, we will start
    -- the list at a previously known large prime number
    --listofprimes = esieve [2..]

    --Then, choose from this list of primes a number that is coprime to the one
    -- provided. 
    --I'm not 100% sure, but I think that keys on the order of ~80 bits long
    -- are more desirable: the longer the key the more secure it is
    --However, this implementation is exceedingly inefficient, so keeping small
    --e = head $ dropWhile (< 2^10) (filter (\x -> mod t x /= 0) listofprimes)
    --Generate very large random numbers (~2^1024) and check to see if it's
    -- coprime with t
    --TODO

--In the process of trying to understand the math behind this; found this
-- method on http://rosettacode.org/wiki/Modular_inverse#Haskell  
--modmultinv e 1 = 1
modmultinv = modInv

-- Extended Euclidean algorithm.  Given non-negative a and b, return x, y and g
-- such that ax + by = g, where g = gcd(a,b).  Note that x or y may be negative.
gcdExt a 0 = (1, 0, a)
gcdExt a b = let (q, r) = a `quotRem` b
                 (s, t, g) = gcdExt b r
             in (t, s - q * t, g)
 
-- Given a and m, return Just x such that ax = 1 mod m.  If there is no such x
-- return Nothing.
modInv a m = let (i, _, g) = gcdExt a m
             in (mkPos i) 
  where mkPos x = if x < 0 then x + m else x

main :: IO ()
main = do
    args <- getArgs
    case args of
        [primeone, primetwo] -> if primeone /= primetwo
                                    then do
                                            let (pubkey, privkey) = 
                                                    genkeys (read primeone)
                                                            (read primetwo)
                                            print $ "Public key:" 
                                                    ++ show pubkey
                                            print $ "Private key:" 
                                                    ++ show privkey
                                    else do
                                        print "The two primes must be distinct."
        _ -> do
            print "Please provide two prime numbers as arguments."                            
