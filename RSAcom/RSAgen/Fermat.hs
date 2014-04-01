--fermat: A module that will apply a test to see if a number is probably prime

module Fermat where

--Import some modules so we can get some random numbers
import System.Random

--Generate a list of random numbers in a range
randgen :: Integer -> Integer -> [Integer]
randgen lo hi = do
	let lout = fmap (randomRs (lo, hi)) newStdGen
	out <- lout
	return out	

--Apply fermat's primality test with all numbers 1-20, then 10 random numbers
-- that are larger
fermattest :: Integer -> Bool
fermattest num = smalls && bigs	where
	smalls = all (\x -> mod (x^(num - 1)) num == 1) [1..20]
	bigs = all (\x -> mod (x^(num - 1)) num == 1) (randgen 0 num)
