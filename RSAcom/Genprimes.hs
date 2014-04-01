-- Genprimes.hs
-- A module that generates 2 ~512 bit prime numbers

module Genprimes where

import System.Random --To generate random numbers
import Expsq --To do exponentiation by squares
import Data.Bool --To use the Bool datatype

--This function will generate random numbers around 2^512 size and test each
-- such that if it passes it has a chance around 1 - 2 ^ (-100)
-- of being prime and returns two such numbers that are unequal
genprimes :: IO (Integer, Integer)
genprimes = do
    rndgen <- getStdGen
    let rnums = randnums rndgen (2^511)
    numone <- gennumsuntil fermat
    numtwo <- gennumsuntil (\x y -> x /= numone && fermat x y)
    return (numone, numtwo)

--Generates random numbers until one that satisfies the given function is
-- found; we use it to find a large prime number
gennumsuntil :: (Integer -> [Integer] -> Bool) -> IO Integer
gennumsuntil fermat = do
    rndgen <- getStdGen
    let rnums = randnums rndgen (2^511)
    candidate <- randbignum
    if (fermat candidate rnums)
        then do
            return candidate
        else do
            gennumsuntil fermat

--Generates a random number between 2^512 and 2^511, giving 
-- ~6.7 x 10^153 possibilities
randbignum :: IO Integer
randbignum = randomRIO( 2^511, 2^512 )

--Generates 100 random numbers between the 1 and 1 - the one given for use in
-- the Fermat test
randnums :: StdGen -> Integer -> [Integer]
randnums g n = map abs $ take 100 $ dropWhile (>= n) $ randoms g

--Uses the Fermat test on a number, given a list of test numbers,
-- (of size 100 to get the desired probability)
fermat :: Integer -> [Integer] -> Bool
fermat n tests = final where
    final = all (\x -> expsq x (n - 1) n == 1) tests
