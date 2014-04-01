-- RSAcrypt.hs
-- A module that allows encryption and decryption of strings passed to it using
-- provided RSA keys.

module RSAcrypt where

--Our fast exponentiation library
import Expsq
--So that we can get the unicode number of a character
import Data.Char

encrypt :: String -> Integer -> Integer -> [Integer]
encrypt msg key mod = encodedstring where
    --This will encrypt each unicode character individually using its unicode
    -- designation
    --fromIntegral is needed as ord returns type Int and expsq takes type
    -- Integer
    encodedstring = map (
                        \x -> expsq (fromIntegral (ord x)) key mod
                        ) msg

decrypt :: [Integer] -> Integer -> Integer -> String
decrypt cyt key mod = decodedstring where
    --Given a list of encrypted characters, will decrypt them and return a
    -- string
    --toEnum takes type Int, so fromIntegral is required
    decodedstring = map (\x -> toEnum (fromIntegral (expsq x key mod))) cyt 
