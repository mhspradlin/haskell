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
import Control.Concurrent.MVar --So we can pass messages and other information
                               -- between threads
import Data.Bool --So we can use the Bool datatype
import RSAcrypt --Our RSA encryption/decryption library
import Keygen (genkeys) --Our RSA key generation library
import Genprimes (genprimes) --Our large prime number generator
import Termdisp (dispMessages) --Our terminal display manager

--Default port number
port = 7707
greeting = "Chat Client 1.1"

main :: IO ()
main = withSocketsDo $ do --For Windows compatibility
    hSetBuffering stdout NoBuffering --Again, for Windows compatibility
    let messages = "" --Initializing our messages list
    dispMessages greeting "" messages
    --Get the IP address of the user
    ip <- getIp (messages, "")
    --Get a port from the user
    port <- getPort port (messages, "")
    let subtitle = "Your address is: " ++ ip ++ ":" ++ show port
    dispTerm greeting subtitle messages
    --The public key, private key, and modulo associated with an RSA keypair
    (p,q) <- genprimes
    let (modulo, pubkey, privkey) = genkeys p q
    --Create a socket that is set to listen for incoming connections
    sock <- listenOn $ PortNumber (fromIntegral port)
    --Create an MVar for our messages and subtitle
    msgs <- newMVar (messages, subtitle)
    loop sock False Nothing (pubkey, privkey, modulo) Nothing msgs

loop :: Socket -> MVar Bool -> Maybe Handle -> (Integer, Integer, Integer) ->
                    Maybe (Integer, Integer) -> MVar ([[Char]],[Char]) -> IO ()
loop sock connstatus h (mypubkey, myprivkey, mymodulo) k msgs = do
    checkconnstatus <- readMVar connstatus
    if checkconnstatus
        then do
            putStr "msg> "
        else do
            putStr "cmd> "
    cmd <- getLine
    --Grab the connection status again
    checkconnstatus <- readMVar connstatus
    if (not checkconnstatus) --If we're not connected
        then 
            if (cmd =~ ("connect [0-9]{1,3}\\.[0-9]{1,3}" ++
                            "\\.[0-9]{1,3}\\.[0-9]{1,3}:[0-9]{1,5}") :: Bool)
                then do
                    let connaddr = takeWhile (/= ':') (drop 8 cmd)
                    let connport = PortNumber $ fromIntegral 
                                             (read (tail $ dropWhile (/= ':') 
                                                                         cmd))
                    makeConnection sock connaddr connport 
                                      (mypubkey, myprivkey, mymodulo) msgs
                else if (cmd =~ "[Ll]isten")
                    then do 
                        getConnection sock (mypubkey, myprivkey, mymodulo) msgs
                    else if (cmd =~ "[Qq]uit")
                        then do
                            return ()
                        else do --Unrecognized command
                            addMsg "Unrecognized input" msgs
                            loop sock False h 
                                (mypubkey, myprivkey, mymodulo) k msgs
        else do
            if (cmd =~ ":[Qq]uit") --Allow the user to exit in a civil manner
                then do
                    --Let the other party know we're leaving
                    addMsg "Connection terminated." msgs
                    let bye = "Terminating Connection"
                    hPutStrLn (fromJust h) (makeMsg bye k)
                    --Send the ASCII 'end of transmission' character to sign off
                    hPutStrLn (fromJust h) (makeMsg "\EOT" k)
                    --Set our connection status to false
                    newstatus <- newMVar False
                    --Return to the command loop
                    loop sock newstatus Nothing (mypubkey, myprivkey, mymodulo) 
                                                            Nothing msgs
                else do --Then we're connected, and we should send the input as 
                        -- a message
                    hPutStrLn (fromJust h) (makeMsg cmd k)
                    addMsg (makeMsg cmd k) msgs
                    loop sock connstatus h (mypubkey, myprivkey, mymodulo) k
                                                                            msgs

getIp :: MVar ([[Char]], [Char]) -> IO String
getIp msgs = do
    putStr "What is your IP address? "
    ip <- getLine
    if (ip =~ "[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}" :: Bool)
        then do
            return ip
        else do
            addMsg "That's an invalid input!" msgs
            ip <- getIp msgs
            return ip

--This handles the retrieval of an encrypted message from a connection
getmsgs :: Socket -> Handle -> (Integer, Integer, Integer) ->
                    MVar ([[Char]],[Char]) -> MVar Bool -> IO ()
getmsgs sock h (pubkey, privkey, modulo) msgs connstatus = do
    checkconnstatus <- readMVar connstatus
    if (not checkconnstatus) --If we're not connected anymore
        --End this thread
        --The connection status has already been set to false
        then do 
            return ()
        --Otherwise keep going
        else do
            str <- hGetLine h --This blocks until something is available, so no need
                              -- to delay this loop
		    --putStr $ "\n" ++ str ++ "" --For illustrative purposes
		    let decryptedmessage = decrypt (parseCyt str) privkey modulo
		    if (decryptedmessage == "\EOT") --The other side has signed off 
		        then do --End this thread, and set the connection status to false
		            checkconnstatus <- takeMVar connstatus
		            putMVar connstatus False
		            return ()
		        else do
		            addMsg decryptedmessage msgs
		            putStr "msg>"
		            getmsgs sock h (pubkey, privkey, modulo) msgs

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
getPort :: Int -> MVar ([[Char]], [Char]) -> IO Int
getPort i msgs = do
    putStr "Use default port? (y/n) "
    ans <- getLine
    if (ans == "y")
        then do
            addMsg ("Using port" ++ show port) msgs
            return i
        else do
            addMsg "Using user-defined port" msgs
            putStr "What port would you like to use? (> 5000) "
            ans <- getLine
            if (read ans > 5000)
            then do 
                addMsg ("Using port" ++ read ans) msgs
                return (read ans)
            else do
                addMsg "Not a valid input." msgs
                port <- getPort i msgs
                return port

--This will encrypt the message and output it as a list of integers
-- expressed as a string like "[12342,254756,37568,4576857]"
makeMsg :: String -> Maybe (Integer, Integer) -> String
makeMsg _ Nothing = "Error: No key provided"
makeMsg input (Just (key, modulo)) = msg where
    msg = show (encrypt input key modulo)

--This handles waiting for a connection and the exchange of public keys once a
-- connection is made as well as kicking off the asynchronous message recieve
-- loop
getConnection :: Socket -> (Integer, Integer, Integer) -> MVar ([[Char]],[Char]) 
                                                                    -> IO ()
getConnection sock (mypubkey, myprivkey, mymodulo) (messages, subtitle) msgs 
                                                                        = do
    addMsg = "Listening..." msgs
    (h, hostname, portnum) <- accept sock
    addMsg ("Recieved connection from " ++ hostname ++":" ++ show portnum) 
                                                                        msgs
    --Send our public key and modulo
    addMsg "Transmitting public key..." msgs
    hPutStrLn h $ (show mypubkey) ++ " " ++ show mymodulo
    --Get their public key and modulo
    addMsg "Waiting for partner public key..." msgs
    keystr <- hGetLine h
    addMsg "Keys exchanged." msgs
    let [theirpubkey, theirmodulo] = map read (words keystr)
    async $ (getmsgs sock h (mypubkey, myprivkey, mymodulo))
    loop sock True (Just h) (mypubkey, myprivkey, mymodulo)
                (Just (theirpubkey, theirmodulo)) msgs

--This handles the making of a connection and the exchange of public keys if the
-- connection is successfully made as well as kicking off the asynchronous
-- message recieve loop
makeConnection :: Socket -> String -> PortID -> 
                            (Integer, Integer, Integer) -> 
                            MVar ([[Char]], [Char]) -> IO ()
makeConnection sock connaddr connport (mypubkey, myprivkey, mymodulo) msgs = do
    addMsg ("Connecting to: " ++ connaddr ++ ":" ++ show connport) msgs
    h <- connectTo connaddr connport
    --Get their public key
    addMsg "Getting partner public key..." msgs
    keystr <- hGetLine h
    let [theirpubkey, theirmodulo] = map read (words keystr)
    --Send our public key
    addMsg "Transmitting public key..." msgs
    hPutStrLn h $ (show mypubkey) ++ " " ++ show mymodulo
    addMsg "Keys exchanged." msgs
    async $ (getmsgs sock h (mypubkey, myprivkey, mymodulo))
    loop sock True (Just h) (mypubkey, myprivkey, mymodulo)
                            (Just (theirpubkey, theirmodulo)) msgs

--This handles the commonly used 'add a message and redisplay the screen'
-- command
--The msgs is passed in the form MVar (messages, subtitle)
addMsg :: [Char] -> MVar ([[Char]], [Char]) -> IO ()
addMsg newmsg msgs = do
    oldmsgs <- takeMVar msgs
    let oldmessages = fst oldmsgs
    let messages = newmsg : oldmessages
    let subtitle = snd oldmsgs
    dispMessages greeting subtitle messages
    --Now we need to update the MVar that holds the messages
    putMVar msgs (messages,subtitle)
