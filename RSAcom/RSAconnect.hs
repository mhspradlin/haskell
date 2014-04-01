-- RSAconnect.hs
-- A program for sending encrypted messages and recieving encrypted
-- messages.

module Main where

import Network --The Haskell networking library
import System.IO --To use the h-Put/Get commands
import Text.Regex.Posix --To do regular expression matching
import Data.Maybe --For the maybe monad
import Control.Concurrent.Async --So we can be reading in messages at the same 
                                -- time as writing our own
import Data.Bool --So we can use the Bool datatype
import RSAcrypt --Our RSA encryption/decryption library
import Keygen (genkeys) --Our RSA key generation library
import Genprimes (genprimes) --Our large prime number generator

--Default port number
port = 7707

main :: IO ()
main = withSocketsDo $ do --For Windows compatibility
    hSetBuffering stdout NoBuffering --Again, for Windows compatibility
    putStrLn "Chat client 1.0"
    --Get the IP address of the user
    ip <- getIp
    --Get a port from the user
    port <- getPort port
    putStrLn $ "Your address is: " ++ ip ++ ":" ++ show port
    --The public key, private key, and modulo associated with an RSA keypair
    (p,q) <- genprimes
    let (modulo, pubkey, privkey) = genkeys p q
    --Create a socket that is set to listen for incoming connections
    sock <- listenOn $ PortNumber (fromIntegral port)
    loop sock False Nothing (pubkey, privkey, modulo) Nothing

loop :: Socket -> Bool -> Maybe Handle -> (Integer, Integer, Integer) ->
                    Maybe (Integer, Integer) -> IO ()
loop sock connstatus h (mypubkey, myprivkey, mymodulo) k = do
    if connstatus
        then do
            putStr "msg> "
        else do
            putStr "cmd> "
    cmd <- getLine
    if (not connstatus) --If we're not connected
        then 
            if (cmd =~ ("connect [0-9]{1,3}\\.[0-9]{1,3}" ++
                            "\\.[0-9]{1,3}\\.[0-9]{1,3}:[0-9]{1,5}") :: Bool)
                then do
                    let connaddr = takeWhile (/= ':') (drop 8 cmd)
                    let connport = PortNumber $ fromIntegral 
                                             (read (tail $ dropWhile (/= ':') 
                                                                         cmd))
                    makeConnection sock connaddr connport 
                                      (mypubkey, myprivkey, mymodulo)
                else if (cmd =~ "[Ll]isten")
                    then do 
                        getConnection sock (mypubkey, myprivkey, mymodulo)
                    else if (cmd =~ "[Qq]uit")
                        then do
                            return ()
                        else do --Unrecognized command
                            putStrLn "Unrecognized input"
                            loop sock False h 
                                (mypubkey, myprivkey, mymodulo) k
        else do
            if (cmd =~ ":[Qq]uit") --Allow the user to exit in a civil manner
                then do
                    --Let the other party know we're leaving
                    let bye = "Connection terminated."
                    hPutStrLn (fromJust h) (makeMsg bye k)
                    --Send the ASCII 'end of transmission' character to sign off
                    hPutStrLn (fromJust h) (makeMsg "\EOT" k)
                    --Return to the command loop
                    loop sock False Nothing (mypubkey, myprivkey, mymodulo) 
                                                                    Nothing
                else do --Then we're connected, and we should send the input as 
                        -- a message
                    hPutStrLn (fromJust h) (makeMsg cmd k)
                    loop sock connstatus h (mypubkey, myprivkey, mymodulo) k

getIp :: IO String
getIp = do
    putStr "What is your IP address? "
    ip <- getLine
    if (ip =~ "[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}" :: Bool)
        then do
            return ip
        else do
            putStrLn "That's an invalid input!"
            ip <- getIp 
            return ip

--This handles the retrieval of an encrypted message from a connection
getmsgs :: Socket -> Handle -> (Integer, Integer, Integer) -> IO ()
getmsgs sock h (pubkey, privkey, modulo) = do
    str <- hGetLine h --This blocks until something is available, so no need
                      -- to delay this loop
    putStr $ "\n" ++ str ++ "" --For illustrative purposes
    let decryptedmessage = decrypt (parseCyt str) privkey modulo
    if (decryptedmessage == "\EOT") --The other side has signed off 
        then do --End this thread
            putStr "\n"
            return ()
        else do
            putStr $ "\n" ++ decryptedmessage ++ "\nmsg> "
            getmsgs sock h (pubkey, privkey, modulo)

--This converts an encrypted message from the string list format like "[1,2,3]"
-- to an Integer list format like [1,2,3]
parseCyt :: String -> [Integer]
parseCyt cyt = list where
    --Strip the leading and trailing brackets
    stripped = (reverse . tail . reverse . tail) cyt
    --Now split the list at all of the commas, a la words from Data.String and
    -- convert them to integers during
    list = nums stripped
    nums s = case dropWhile (== ',') s of
        "" -> []
        nonempty -> (read num :: Integer) : nums onelessnum where
            (num, onelessnum) = break (== ',') nonempty

--This handles getting the user to properly input a port number
getPort :: Int -> IO Int
getPort i = do
    putStr "Use default port? (y/n) "
    ans <- getLine
    if (ans == "y")
        then do
            return i
        else do
            putStr "What port would you like to use? (> 5000) "
            ans <- getLine
            if (read ans > 5000)
            then do 
                return (read ans)
            else do
                putStrLn "Not a valid input."
                getPort i

--This will encrypt the message and output it as a list of integers
-- expressed as a string like "[12342,254756,37568,4576857]"
makeMsg :: String -> Maybe (Integer, Integer) -> String
makeMsg _ Nothing = "Error: No key provided"
makeMsg input (Just (key, modulo)) = msg where
    msg = show (encrypt input key modulo)

--This handles waiting for a connection and the exchange of public keys once a
-- connection is made as well as kicking off the asynchronous message recieve
-- loop
getConnection :: Socket -> (Integer, Integer, Integer) -> IO ()
getConnection sock (mypubkey, myprivkey, mymodulo) = do
    (h, hostname, portnum) <- accept sock
    putStrLn $ "Recieved connection from " ++ hostname ++":" ++ show portnum
    --Send our public key and modulo
    putStrLn "Transmitting public key..."
    hPutStrLn h $ (show mypubkey) ++ " " ++ show mymodulo
    --Get their public key and modulo
    putStrLn "Waiting for partner public key..."
    keystr <- hGetLine h
    putStrLn "Keys exchanged."
    let [theirpubkey, theirmodulo] = map read (words keystr)
    async $ (getmsgs sock h (mypubkey, myprivkey, mymodulo))
    loop sock True (Just h) (mypubkey, myprivkey, mymodulo)
                (Just (theirpubkey, theirmodulo))

--This handles the making of a connection and the exchange of public keys if the
-- connection is successfully made as well as kicking off the asynchronous
-- message recieve loop
makeConnection :: Socket -> String -> PortID -> 
                            (Integer, Integer, Integer) ->  IO ()
makeConnection sock connaddr connport (mypubkey, myprivkey, mymodulo) = do
    putStrLn $ "Connecting to: " ++ connaddr ++ ":" ++ show connport
    h <- connectTo connaddr connport
    --Get their public key
    putStrLn "Getting partner public key..."
    keystr <- hGetLine h
    let [theirpubkey, theirmodulo] = map read (words keystr)
    --Send our public key
    putStrLn "Transmitting public key..."
    hPutStrLn h $ (show mypubkey) ++ " " ++ show mymodulo
    putStrLn "Keys exchanged."
    async $ (getmsgs sock h (mypubkey, myprivkey, mymodulo))
    loop sock True (Just h) (mypubkey, myprivkey, mymodulo)
                            (Just (theirpubkey, theirmodulo))
