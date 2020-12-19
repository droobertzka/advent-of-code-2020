module FileIo
    ( fileIo
    ) where

import System.IO


fileIo filename findAnswer = do
    inh <- openFile filename ReadMode
    mainloop inh ([] :: [String]) findAnswer
    hClose inh

mainloop inh list findAnswer = do
    isEOF <- hIsEOF inh
    if isEOF
        then do
            print $ findAnswer list
        else do
            nextLineIn <- hGetLine inh
            mainloop inh (list ++ [nextLineIn]) findAnswer
