{-# LANGUAGE FlexibleContexts #-}
import Text.Parsec
import Text.Parsec.String (GenParser)
import Text.Parsec.String (Parser)

import Data.List (intercalate)
import Data.Char
import Control.Monad
import Numeric 

--Tree_lc

eol :: Parser Char 
eol = char '\n'

white_space :: Parser Char 
white_space = space <|> newline <|> tab

skip_white_space = skipMany white_space

--skip comment and white space
skip_cws = skipMany $ ((skipMany1 white_space) <|> (skipMany1 comment))

(<->) :: (Parser [Char]) -> (Parser [Char]) -> (Parser [Char])
(<->) a b = 
	do
		x <- a
		skip_cws
		y <- b
		return (x ++ " " ++ y)

line_comment :: Parser String 
line_comment = do
	string "//"
	manyTill anyChar (try eol)

block_comment :: Parser String 
block_comment = do
	string "/*"
	manyTill anyChar (try (string "*/"))

comment = try line_comment <|> block_comment

func_block:: Parser String
func_block = do
	string "func"
	skip_cws
	args <- sepBy lc_token (lc_token_s "->")
	skip_cws
	bodys <- func_body
	return $ "func" ++ (intercalate " -> " args) ++ " | " ++ (bodys)

struct_block:: Parser String
struct_block = do
	string "struct"
	skip_cws
	bodys <- struct_body
	return $ " struct " ++ (bodys)

func_body:: Parser String
func_body = do
	char '{'
	skip_cws
	statements <- many lc_statement
	skip_cws
	char '}'
	return $ intercalate " ; " statements 

struct_body::Parser String
struct_body = do
	char '{'
	skip_cws
	statements <- many lc_statement
	skip_cws
	char '}'
	return $ intercalate " ; " statements 
	

lc_return = do
	(string "return") <-> (manyTill anyChar(try(char ';')))

lc_pre_defined_types = ["metanum", "metastr", "metatime", "metafreq",
	"bit"]

lc_digit:: (Stream s m Char) => Int -> Int -> ParsecT s u m Int 
lc_digit b val = do
	digit <- hexDigit <|> char '_'
	f digit
	where f v
		| v == '_' = return val
		| digitToInt v < b = return $ val*b+digitToInt v 
		| otherwise = unexpected [v] 

-- b is base of number
lc_number_body b val = do
	new <- lc_digit b val
	(try $ lc_number_body_2 b new ) <|> do 
		white_space  
		return new

lc_number::Parser String
lc_number = do
	bit_width <- many digit
	char '\''
	base_code <- oneOf "bBoOdDhH"
	value <- f base_code
	return $ bit_width ++ "-" ++ show value
	where f b
		| elem b "bB" = lc_number_body 2 0
		| elem b "oO" = lc_number_body 8 0
		| elem b "dD" = lc_number_body 10 0
		| elem b "hH" = lc_number_body 16 0

lc_variable_def::Parser String
lc_variable_def = do
	var_type <- lc_token 
	var_name <- lc_token
	char ';'
	return $ var_type ++ " " ++ var_name

lc_statement::Parser String
lc_statement = do
	try func_block <|>
		lc_return

lc_token :: Parser String
lc_token = do
	result <- many1 letter
	skip_white_space
	return result

lc_token_s :: String -> Parser String
lc_token_s s = do
	result <- string s
	skip_white_space
	return result

lc_file :: Parser [String]
--lc_file = line_comment 
lc_file = do
	result <- many $ try block_comment <|> try line_comment <|>
		try func_block
	eof
	return result


--parse_lc :: String -> Either ParseError String 
parse_lc input = parse lc_file "" input
