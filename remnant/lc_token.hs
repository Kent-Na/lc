import Text.Parsec
import Text.Parsec.String (GenParser)
import Text.Parsec.String (Parser)

import Data.List (intercalate)

--String -> [String]

--Tree_lc

eol :: Parser Char 
eol = char '\n'

white_space :: Parser Char 
white_space = space <|> newline <|> tab

skip_white_space = skipMany white_space

line_comment :: Parser String 
--line_comment = between (string "//") eol (many anyChar)
line_comment = do
	string "//"
	manyTill anyChar (try eol)

block_comment :: Parser String 
--block_comment = between (string "/*") (string "*/") (many anyChar)
block_comment = do
	string "/*"
	manyTill anyChar (try (string "*/"))

symbol :: Parser String
symbol = do
	x  <- anyChar
	xs <- manyTill anyChar white_space
	return (x:xs)

lc_token :: Parser String
lc_token = do
	result <- try block_comment <|> 
		try line_comment <|>
		try symbol
	try skip_white_space
	return result


lc_file :: Parser [String]
--lc_file = line_comment 
lc_file = do
	result <- many lc_token
	eof
	return result


--parse_lc :: String -> Either ParseError String 
parse_lc input = parse lc_file "" input
