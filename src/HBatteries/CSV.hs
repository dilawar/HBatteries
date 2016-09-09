module HBatteries.CSV 
    ( 
    readCSV
    ) where

{-# LANGUAGE ScopedTypeVariables #-}
import qualified Data.Csv as C
import qualified Data.Csv.Parser as CP
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lex.Fractional (readDecimal)
import qualified Data.List as L
import Data.Attoparsec.ByteString.Lazy (parse, maybeResult)
import qualified Data.HashMap.Lazy as HL 

type Vec = V.Vector Double

instance C.FromRecord Double

decodeOpt = C.DecodeOptions 32 -- 32 is space

(low, high, thres) = (0, 4, 1) 

-- Get a given column.
{-getColumn :: String ->  -> Vec-}
getColumn columnName (header, res) = V.map (\x ->  fst $ fromJust $ readDecimal x) $
    V.map (\x -> fromJust $ HL.lookup (pack columnName) x ) res

find_transitions vec = find_transitions' 1 [(0, V.head vec == high)] (V.tail vec)
  where 
    find_transitions' i ss v 
        | V.null v = ss
        | highToLow (head ss) (V.head v) = find_transitions' (i+1) ((i,False):ss) (V.tail v)
        | lowToHigh (head ss) (V.head v) = find_transitions' (i+1) ((i,True):ss) (V.tail v)
        | otherwise = find_transitions' (i+1) ss (V.tail v)

lowToHigh (i,False) val = if val >= (high - thres) then True else False
lowToHigh _ _ = False
highToLow (i,True) val = if val <= (low + thres) then True else False
highToLow _ _ = False


-- ! Read a csv file with given delimiter .
readCSV :: String -> Char -> IO ((C.Header,  V.Vector C.NamedRecord))
readCSV filepath delimeter = BS.readFile filepath 
    >>= \text -> return $ fromJust $ maybeResult $ parse (CP.csvWithHeader decodeOpt) text 
  where 
    decodeOpt = C.DecodeOptions (fromIntegral $ fromEnum delimeter)
