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

(<++>) :: (Parser [Char]) -> (Parser [Char]) -> (Parser [Char])
(<++>) a b = 
	do
		x <- a
		skip_cws
		y <- b
		return (x ++ " " ++ y)

(<:>) :: (Parser Char) -> (Parser [Char]) -> (Parser [Char])
(<:>) a b = 
	do
		x <- a
		skip_cws
		y <- b
		return (x : " " ++ y)

as_list :: (Parser a) -> (Parser [a])
as_list a = 
	do
		x <- a
		return [x]

line_comment :: Parser String 
line_comment = do
	string "//"
	manyTill anyChar (try eol)

block_comment :: Parser String 
block_comment = do
	string "/*"
	manyTill anyChar (try (string "*/"))

comment = try line_comment <|> try block_comment

func_block:: Parser String
func_block = do
	string "func"
	skip_cws
	args <- sepBy (choice[
		try (lc_token <++> lc_token), try lc_token])
				(lc_token_s "->")
	skip_cws
	bodys <- func_body
	return $ "func " ++ (intercalate " -> " args) ++ " | " ++ (bodys)

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
	statements <- many $ choice [
		try lc_assign, 
		try lc_return]
	skip_cws
	char '}'
	return $ intercalate " ; " statements 

struct_body::Parser String
struct_body = do
	char '{'
	skip_cws
	statements <- many lc_variable_def 
	skip_cws
	char '}'
	return $ intercalate " ; " statements 
	

lc_return = do
	(string "return") <++> lc_statement <++> (as_list $ char ';')
	--(manyTill anyChar(try(char ';')))

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
		try lc_metanum
		]

lc_variable_def::Parser String
lc_variable_def = do
	var_type <- lc_token 
	var_name <- lc_token
	char ';'
	skip_cws
	return $ var_type ++ " " ++ var_name

--lc_statement::Parser String
--lc_statement = do
	--try func_block <|>
		--lc_return


unary_op = do
	a <- oneOf "-&|"
	skip_cws
	return [a]

binary_op = do
	a <- oneOf "+-*/&"
	skip_cws
	return [a]

lc_assign::Parser String
lc_assign = do 
	l <- lc_statement
	char '='
	skip_cws
	r <- lc_statement
	char ';'
	return $ "assign:" ++ l ++ "=" ++ r

--{a, b, c}
lc_cat_op = do 
	statements <- between (lc_token_s "{") (lc_token_s "}")
		(sepBy1 lc_statement (lc_token_s ","))
	return $ "{" ++ (intercalate " , " statements) ++ "}"

--a[b:c, d]
lc_slice_op = do
	statements <- between (lc_token_s "[") (lc_token_s "]")
		(sepBy1 (choice [
			try (lc_statement <++> lc_token_s ":" <++> lc_statement),
			try (lc_statement)]) 
		(lc_token_s ","))
	return $ "[" ++ (intercalate " , " statements) ++ "]"

lc_statement :: Parser String
lc_statement = do
	choice [
		try (unary_op<++>lc_statement), 
		try lc_cat_op,
		try (lc_token<++>lc_slice_op),
 		try (between (lc_token_s "(") (lc_token_s ")") lc_statement),
 		try (lc_token<++>binary_op<++>lc_statement),
		try lc_token]
	
lc_token :: Parser String
lc_token = do
	result <- many1 (letter <|> char '_')
	skip_cws
	return result

lc_token_s :: String -> Parser String
lc_token_s s = do
	result <- string s
	skip_cws
	return result

lc_file :: Parser [String]
--lc_file = line_comment 
lc_file = do
	result <- many $ try block_comment <|> try line_comment <|>
		try func_block
	eof
	return result


--for debug
kilo_assign= "{lval_a[idx_a, idx_b:idx_c], lval_b} = " ++
	"r_val_a+r_val_b-r_val_c;"
mega_statement = "{lval_a[2, 12:16], lval_b} = " ++
	"r_val_a+r_val_b-r_val_c"

--parse_lc :: String -> Either ParseError String 
parse_lc input = parse lc_file "" input
