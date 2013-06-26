{-# LANGUAGE FlexibleContexts #-}

module Token(
    Unit,
	Token(
        User_token,
        Predefined_token,
        Keyword,
        Symbol,
        Bit_array_literal,
        Meta_number_literal),
    value,
    lc_token, lc_tokenize_file,
) where

import Text.Parsec
import Text.Parsec.Pos (newPos)
import Text.Parsec.String (GenParser)
import Text.Parsec.String (Parser)

import Data.List (intercalate)
import Data.Char
import Control.Monad
import Numeric 

----
--Token

--Keyword : Predefined keywords like "struct" or "func".
--User_token : Identifiers possibly defined by users.
--Symbol : Operators and delimiters like "+" "{" ...
--Bit_array_literal w v: Bit array constants with length w and value v
--                       like 3'b01_1.
--Meta_number_literal v u: Meta number constants with value v and unit u.

type Unit = String

data Token
	=User_token          {value::String}
	|Predefined_token    {value::String} 
	|Keyword             {value::String} 
	|Symbol              {value::String}
	|Bit_array_literal   Int Int 
	|Meta_number_literal Int Unit 
	deriving (Show, Eq)

-------
--Tokenizer

--------
--comment and white space
white_space :: Parser Char 
white_space = space <|> newline <|> tab

skip_white_space = skipMany white_space

double_char_symbol = ["->", "=>", "==", ">>", "<<", "++", "--"]
single_char_symbol = [":", "=", "{", "}", "[", "]", ",", "(", ")"] ++
	["+", "-", "*", "/", "^", ".", ";", "&", "|", "~"]


keywords = ["func", "struct", "module", "return", "in", "out"] ++
    ["import"] ++
    ["if", "elif", "else", "on", "posedge", "negedge"]
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

lc_user_token :: Parser (SourcePos, Token)
lc_user_token = do
	pos <- getPosition
	x <- letter
	xs <- many (letter <|> char '_' <|> digit)
	return $ (pos, User_token (x:xs))

---------
--symbols&keywords
lc_symbol = do
	pos <- getPosition
	x <- choice (map (try.string) double_char_symbol) <|>
	     choice (map (try.string) single_char_symbol)
	return $ (pos, Symbol x)

lc_keyword = do
	pos <- getPosition
	x <- choice (map (try.string) keywords)
	return $ (pos, Keyword x)

lc_predefined_token= do
	pos <- getPosition
	x <- choice (map (try.string) pre_defined_tyes) <|>
	     choice (map (try.string) pre_defined_meta_tyes)
	return $ (pos, Predefined_token x)

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
		return new

lc_number::Parser (SourcePos, Token)
lc_number = do
	pos <- getPosition
	bit_width <- many digit
	char '\''
	base_code <- oneOf "bBoOdDhH"
	value <- f base_code
	return $ (pos, Bit_array_literal (read bit_width) value)
	where f b
		| elem b "bB" = lc_number_body 2 0
		| elem b "oO" = lc_number_body 8 0
		| elem b "dD" = lc_number_body 10 0
		| elem b "hH" = lc_number_body 16 0

lc_metanum_zero::Parser Int
lc_metanum_zero = do 
	msb <- char '0'
	return 0

lc_metanum_body::Parser Int 
lc_metanum_body = do 
	msb <- oneOf "123456789"
	lest <- many digit
	return $ read (msb:lest)

lc_metanum = do 
	pos <- getPosition
	val <- try lc_metanum_zero <|> lc_metanum_body
	return $ (pos, Meta_number_literal val "")

si_prefix::Parser Char 
si_prefix = oneOf "TGMkmunp"

lc_metafreq = do
	pos <- getPosition
	val <- try lc_metanum_zero <|> lc_metanum_body
	unit <- choice [
		try $ do {p<-si_prefix; u<-string "Hz"; return (p:u)},
		try $ string "Hz"]
	return $ (pos, Meta_number_literal val unit)

lc_metatime = do
	pos <- getPosition
	val <- try lc_metanum_zero <|> lc_metanum_body
	unit <- choice [
		try $ do {p<-si_prefix; u<-string "s"; return (p:u)},
		try $ string "s"]
	return $ (pos, Meta_number_literal val unit)

lc_literal::Parser (SourcePos, Token)
lc_literal = choice [
		try lc_number,
		try lc_metafreq,
		try lc_metatime,
		lc_metanum
		]

lc_token::Parser (SourcePos, Token)
lc_token = do
	t <- choice [
		lc_symbol,
		lc_keyword,
		lc_predefined_token,
		try lc_literal,
		lc_user_token
		]
	skip_cws
	return t

lc_tokenize_file = do
	skip_cws
	contents <- many lc_token
	eof
	return contents 

----
--for debug

load_test = readFile "tests/func.lc"
do_test s = parse lc_tokenize_file "" s

kilo_assign= "{lval_a[idx_a, idx_b:idx_c], lval_b} = " ++
	"r_val_a+r_val_b-r_val_c;"
mega_statement = "{lval_a[2, 12:16], lval_b} = " ++
	"r_val_a+r_val_b-r_val_c"

fancy_show (Right s) = show (snd (unzip s))
fancy_show (Left s) = "err"
