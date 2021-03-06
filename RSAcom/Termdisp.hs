-- termdisp.hs
-- A package for the terminal control commands in the System.Console.ANSI
-- package.

module Termdisp where

--Our terminal control commands
import System.Console.ANSI
--Other IO stuff
import System.IO
--To get the size of the terminal
import System.Console.Terminal.Size
--The Maybe Monad (for the Size package)
import Data.Maybe

--Gets the size of the terminal
getSize :: IO (Int, Int)
getSize = do
    window <- size
    let fixwindow = maybe (Window {height = 0, width = 0}) (\x -> x) window
    return (height fixwindow, width fixwindow)

--Given screen height and width, picks out a sublist of strings that, when
-- rendered, will stay on the screen (minus three lines off of the top)
pickDisplayMessages :: Int -> Int -> [[Char]] -> [[Char]]
pickDisplayMessages height width messages = dispmessages where
    listoflens = map lineno messages where
        --Determines how many lines will be taken up by a message
        --Does fractional division, then rounds up using ceiling
        lineno :: [Char] -> Int
        lineno msg = ceiling $ (fromIntegral (length msg)) / 
                                        (fromIntegral width)
    --Accumulates the list
    lensums = scanl1 (+) listoflens
    entstotake = length $ takeWhile (< (height - 3)) lensums
    dispmessages = take entstotake messages

--Will print a message, placing the bottom part of the message where the cursor
-- currently is and leaving the cursor on the line above the start of the 
-- message
printMessage :: Int -> Int -> String -> IO ()
printMessage height width message = do
    setCursorColumn 0
    if (length message) `div` width > 0
        then do
            cursorUpLine $ (length message) `div` width
        else do
            return () --Don't do anything, since cursorUpLine 0 moves it up 1
    putStr message 
    cursorUpLine $ (length message) `div` width + 1
    setCursorColumn 0

--Will take a list of IO commands and run them one after another
execList :: [IO ()] -> IO ()
execList [] = return ()
execList (cmd : cmds) = do 
    cmd
    execList cmds

--Our main function, which chooses from the list of all messages which ones to
-- display and constructs the display with header and all, leaving the bottom line
-- blank
dispMessages :: [Char] -> [Char] -> [[Char]] -> IO ()
dispMessages greeting subtitle messages = do
    --Clear the screen, then print a greeting message centered at the top
    clearScreen
    (rows, cols) <- getSize
    setCursorPosition 0  $ (cols - (length greeting)) `div` 2
    putStr greeting
    setCursorPosition 2 $ (cols - (length subtitle)) `div` 2
    putStr subtitle
    --Print the messages, starting at one above the bottom of the screen, and 
    -- stopping when it would collide with the three lines at the top
    setCursorPosition (rows - 2) 0
    execList $ map (printMessage rows cols) 
        (pickDisplayMessages rows cols messages)

    --Put the cursor back at the bottom of the screen
    setCursorPosition rows 0
