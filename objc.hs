-- Obj-C Style Checker
-- Juan C. Montemayor Elosua
-- j.mont@me.com

import Control.Monad

import Text.ParserCombinators.Parsec

-- Syntax
commentLine = "//"
commentStart = "/*"
commentEnd = "*/"

eol :: GenParser Char st Char
eol = char '\n'

-- File
objCFile :: GenParser Char st [String]
objCFile = do 
    result <- comment
    eof
    return [result]

-- Comments
singleComment :: GenParser Char st String
singleComment = string "//" >> space >>  many (noneOf "\n")

multiComment :: GenParser Char st String
multiComment = string commentStart >> space >> manyTill anyChar (try (space >> string commentEnd))

comment = try singleComment <|> try multiComment
-- Parse
parseObjC :: String -> Either ParseError [String]
parseObjC input = parse objCFile "(unknown)" input
