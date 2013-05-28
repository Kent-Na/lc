{-# LANGUAGE FlexibleContexts #-}
import Text.Parsec
import Text.Parsec.String (GenParser)
import Text.Parsec.String (Parser)

import Data.List (intercalate)
import Data.Char
import Control.Monad
import Numeric 


--Keyword : Predefined keywords like "struct" or "func".
--User_token : Identifiers possibly defined by users.
--Symbol : Operators and delimiters like "+" "{" ...
--Bit_array_literal w v: Bit array constants With length w and value v
--                       like 3'b01_1.
--Meta_number_literal v u: Meta number constants With value v and unit u.


data Token = 
	User_token String |
	Keyword String |
	Symbol String |
	Bit_array_literal Int Int |
	Meta_number_literal Int String
	
white_space :: Parser Char 
white_space = space <|> newline <|> tab

skip_white_space = skipMany white_space

double_char_symbol = ["->", "=>"]
single_char_symbol = [":", "=", "{", "}", "[", "]", ","] ++
	["+", "-", "*", "/", ";"]

keywords = ["func", "struct", "module", "return", "in", "out"]
pre_defined_tyes = ["bit", "logic", "auto"]
pre_defined_meta_tyes = ["metanum", "metastr", "metatime", "metafreq"]

line_comment :: Parser String 
line_comment = do
	string "//"
	manyTill anyChar (try newline)

comment = try line_comment <|> block_comment

block_comment :: Parser String 
block_comment = do
	string "/*"
	manyTill anyChar (try (string "*/"))

--skip comment and white space
skip_cws = skipMany $ ((skipMany1 white_space) <|> (skipMany1 comment))

lc_user_token :: Parser String
lc_user_token = do
	x <- letter
	xs <- many (letter <|> char '_' <|> digit)
	return (x:xs) 

-------------------
---Number literals

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
	(try $ lc_number_body b new ) <|> do 
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

lc_metanum::Parser String
lc_metanum = do 
	msb <- oneOf "123456789"
	lest <- many digit
	return (msb:lest)

si_prefix::Parser Char 
si_prefix = oneOf "TGMkmunp"

lc_metafreq = do
	val <- lc_metanum
	unit <- choice [
		try $ do {p<-si_prefix; u<-string "Hz"; return (p:u)},
		try $ string "Hz"]
	return $ val ++ unit

lc_metatime = do
	val <- lc_metanum
	unit <- choice [
		try $ do {p<-si_prefix; u<-string "s"; return (p:u)},
		try $ string "s"]
	return $ val ++ unit

lc_literal::Parser String
lc_literal = choice [
		try lc_number,
		try lc_metafreq,
		try lc_metatime,
		lc_metanum
		]

lc_token = do
	t <- choice [
		choice (map (try.string) double_char_symbol),
		choice (map (try.string) single_char_symbol),
		choice (map (try.string) keywords),
		choice (map (try.string) pre_defined_tyes),
		choice (map (try.string) pre_defined_meta_tyes),
		try lc_literal,
		lc_user_token
		]
	skip_cws
	return t

lc_tokens::[String] -> Int -> Parser [String]
lc_tokens s n  
	| n == 0 = do {last<-lc_token; return (s++[last])}
	| otherwise = do {x<-lc_token; lc_tokens (s++[x]) (n-1);}

lc_file = do
	skip_cws
	contents <- many lc_token
	eof
	return contents 

load_test = readFile "tests/func.lc"
do_test s = parse lc_file "" s

kilo_assign= "{lval_a[idx_a, idx_b:idx_c], lval_b} = " ++
	"r_val_a+r_val_b-r_val_c;"
mega_statement = "{lval_a[2, 12:16], lval_b} = " ++
	"r_val_a+r_val_b-r_val_c"
