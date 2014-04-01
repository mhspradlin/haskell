-- connect.hs
-- A notebook for testing haskell networking functions.

module Main where

import Network
import System.IO
import Text.Regex.Posix --To do regular expression matching
import Data.Maybe
import Control.Concurrent.Async --So we can be reading in messages at the same time as
              -- writing our own
import Data.Bool --So we can use the Bool datatype

--Default port number
port = 7707
--Keeps track of if we're connected to someone or not
connstatus = False

main :: IO ()
main = withSocketsDo $ do --For Windows compatibility
    hSetBuffering stdout NoBuffering --Again, for Windows compatibility
    putStrLn "Chat client 1.0"
    --Get the IP address of the user
    ip <- getIp
    --Get a port from the user
    port <- getPort port
    putStrLn $ "Your address is: " ++ ip ++ ":" ++ show port
    --Create a socket that is set to listen for incoming connections
    sock <- listenOn $ PortNumber (fromIntegral port)
    loop sock connstatus Nothing

loop :: Socket -> Bool -> Maybe Handle -> IO ()
loop sock connstatus h = do
    putStr "> "
    cmd <- getLine
    if (not connstatus) --If we're not connected
    then
        if (cmd =~ "connect [0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}:[0-9]{1,5}")
        then do
            let connport = PortNumber $ fromIntegral (read (tail $ dropWhile (/= ':') cmd))
            putStrLn $ "Connecting to: " ++ (takeWhile (/= ':') (drop 8 cmd)) ++ (show connport)
            h <- connectTo (takeWhile (/= ':') (drop 8 cmd)) 
                   (connport)
            async $ (getmsgs h)
            loop sock True (Just h)
        else if (cmd =~ "[Ll]isten")
            then do 
            --Wait until we recieve a connection
            (h, hostName, portNum) <- accept sock
            putStrLn $ "Recieved connection from " ++ hostName ++":" ++ show portNum
            async $ (getmsgs h)
            loop sock True (Just h)
            else if (cmd =~ "[Qq]uit")
            then do
                return ()
            else do --Unrecognized command
                putStrLn "Unrecognized input"
                loop sock connstatus h
    else do --Then we're connected, and we should send the input as a message
        hPutStrLn (fromJust h) cmd
        loop sock connstatus h
{-
    (h, _, _) <- accept sock
    hPutStr h "Hey there!\n"
    hPutStr h "How are you?\n"
    interface h
    hPutStr h "Quitting...\n"
    hFlush h
    hClose h


interface :: Handle -> IO ()
interface h = do
    str <- hGetLine h
    maybeQuit str h

maybeQuit :: String -> Handle -> IO ()
maybeQuit str h
    | str == "quit\r" = do
    putStrLn "Quitting..."
    | otherwise = do
    putStrLn $ (reverse . tail . reverse) str
    interface h
-}

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

getmsgs :: Handle -> IO ()
getmsgs h = do
    str <- hGetLine h
    putStr $ "\n" ++ str ++ "\n> "
    getmsgs h

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
