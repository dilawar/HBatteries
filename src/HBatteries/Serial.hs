module HBatteries.Serial  (
    fetchLine
    , fetchLines 
    ) where

import System.IO
import System.Hardware.Serialport
import Control.Monad 
import qualified Data.ByteString.Char8 as B
import System.Exit

eol_ = B.singleton '\n'

-- | Convert to CommSpeed 
speed :: String -> CommSpeed 
speed x | x == "9600" = CS9600 
        | x == "19200" = CS19200 
        | x == "38400" = CS38400 
        | x == "57600" = CS57600 
        | x == "115200" = CS115200 
        | otherwise = CS9600


-- | Fetch a single line from usb-port s.
fetchLine s = do 
    c <- recv s 1
    if c == eol_ then
        return B.empty
    else do
        vs <- fetchLine s
        return $! B.append c vs

-- | Fetch as many lines as possible from ubs-port h.
fetchLines h = do 
    fetchLine h >>= print 
    isDataAvailable <- hReady stdin
    if isDataAvailable then do
        char <- hGetChar stdin
        case char of 
            '\^]' -> exitSuccess 
            _ -> send h $ B.singleton char
      else do
        return 0
