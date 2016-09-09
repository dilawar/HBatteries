module HBatteries.CSV 
    ( 
    readCSV
    , getColumn
    ) where

{-# LANGUAGE ScopedTypeVariables #-}
import qualified Data.Csv as C
import qualified Data.Csv.Parser as CP
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Lex.Fractional (readDecimal)
import qualified Data.List as L
import Data.Attoparsec.ByteString.Lazy (parse, maybeResult)
import qualified Data.HashMap.Strict as HL 
import Data.ByteString.Lazy.Internal ( packChars )
import qualified Data.ByteString.Internal as BS (c2w, w2c)

type Vec = V.Vector Double

instance C.FromRecord Double

-- | Get a given column from results of readCSV
getColumn columnName (header, res) = 
    V.map (\x ->  fst $ fromJust $ readDecimal x) $
    V.map (\x -> x HL.! (C.pack columnName) ) res

-- ! Read a csv file into a vector of double. Need to specify the delimiter.
-- Any line starting with '#' will be ignored.
readCSV :: String -> Char -> IO ((C.Header,  V.Vector C.NamedRecord))
readCSV filepath delimeter = do 
    text <- BS.readFile filepath
    let lines = filter (not . BS.null) $ BS.split (BS.c2w '\n') text
    -- Ignore all lines starting with '#'
    let filtered = filter (\l -> BS.index l 0 /= BS.c2w '#' ) lines
    let t = BS.intercalate (packChars "\n") filtered
    return . fromJust . maybeResult $ parse (CP.csvWithHeader decodeOpt) t 
  where 
    decodeOpt = C.DecodeOptions (BS.c2w delimeter)


