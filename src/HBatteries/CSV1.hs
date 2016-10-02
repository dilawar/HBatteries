{-# LANGUAGE Safe, RankNTypes #-}

module HBatteries.CSV1 ( parseCSV ) where 

import Text.ParserCombinators.Parsec
import Data.List (intersperse)

eol :: forall st. GenParser Char st String
eol = (try $ string "\n\r") <|> (try $ string "\r\n") <|> string "\n" <|>
      string "\r" <?> "End of line"

cell :: GenParser Char st String
cell = quotedcell <|> many (noneOf ",\n\r")

quotedchar :: GenParser Char st Char
quotedchar = noneOf "\""
             <|> (try $ do string "\"\""
                           return '"'
                 )
quotedcell :: CharParser st String
quotedcell = do char '"'
                content <- many quotedchar
                char '"'
                return content

line :: Char -> GenParser Char st [String]
line delimiter = sepBy cell (char delimiter)

csvFile :: Char -> CharParser st [[String]]
csvFile x = endBy (line  x) eol

-- | parse csv file using Parsec 
parseCSV :: Char -> String -> IO (Either ParseError [[ String ]])
parseCSV delimiter filepath = parseFromFile (csvFile delimiter) filepath
