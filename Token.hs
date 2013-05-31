{-# LANGUAGE FlexibleContexts #-}

module Token(
	Token(
        User_token,
        Predefined_token,
        Keyword,
        Symbol,
        Bit_array_literal,
        Meta_number_literal),
    lc_token, lc_tokenize_file,
    Token_parser,
    satisfy_token, any_token,
    user_token, s_token,
    unary_op, binary_op,
    (<:>),
    (<+>)
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
--Bit_array_literal w v: Bit array constants With length w and value v
--                       like 3'b01_1.
--Meta_number_literal v u: Meta number constants With value v and unit u.

data Token = 
	User_token          String |
	Predefined_token    String |
	Keyword             String |
	Symbol              String |
	Bit_array_literal   Int Int |
	Meta_number_literal Int String
	deriving (Show, Eq)

-----------
-- parsec token parser

type Token_parser = Parsec [(SourcePos, Token)] ()

is_user_token :: Token -> Bool
is_user_token (User_token s) = True
is_user_token _ = False 

user_token = satisfy_token is_user_token

update_pos_token ((x,_):_) = x
update_pos_token [] = newPos "!endoffile!" 0 0 

satisfy_token :: (Token -> Bool) -> (Token_parser Token)
--(Stream [(SourcePos, Token)] m (SourcePos, Token)) => 
satisfy_token f = tokenPrim (\(pos, tok) -> show tok)
                            (\pos x xs -> update_pos_token xs)
                            (\(pos, tok) -> if f tok then Just tok else Nothing)

any_token :: Token_parser Token
any_token = satisfy_token (const True)

--String token. Parses a token that mach with given string.
s_token :: String -> Token_parser Token
s_token s = 
   f x
   where 
    f (Right (pos, tok)) = satisfy_token (== tok)
    f (Left _ ) = unexpected "bad token (bug in compiler)"
    x = (parse lc_token "" s)

unary_op :: Token_parser Token
unary_op = choice (map s_token ["+", "-", "&", "|"])

binary_op :: Token_parser Token
binary_op = choice (map s_token ["+", "-", "&", "|"])

(<:>) :: (Token_parser a) -> (Token_parser [a]) -> (Token_parser [a])
l <:> r = liftM2 (:) l r
infixr 5 <:>

(<+>) :: (Token_parser a) -> (Token_parser b) -> (Token_parser b)
l <+> r = do
    l
    r
-------
--Tokenizer

--------
--comment and white space
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
		white_space  
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

lc_metanum_body::Parser Int 
lc_metanum_body = do 
	msb <- oneOf "123456789"
	lest <- many digit
	return $ read (msb:lest)

lc_metanum = do 
	pos <- getPosition
	val <- lc_metanum_body
	return $ (pos, Meta_number_literal val "")

si_prefix::Parser Char 
si_prefix = oneOf "TGMkmunp"

lc_metafreq = do
	pos <- getPosition
	val <- lc_metanum_body
	unit <- choice [
		try $ do {p<-si_prefix; u<-string "Hz"; return (p:u)},
		try $ string "Hz"]
	return $ (pos, Meta_number_literal val unit)

lc_metatime = do
	pos <- getPosition
	val <- lc_metanum_body
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

--lc_tokens::[String] -> Int -> Parser [String]
--lc_tokens s n  
	-- | n == 0 = do {last<-lc_token; return (s++[last])}
	-- | otherwise = do {x<-lc_token; lc_tokens (s++[x]) (n-1);}

lc_tokenize_file = do
	skip_cws
	contents <- many lc_token
	eof
	return contents 

load_test = readFile "tests/func.lc"
do_test s = parse lc_tokenize_file "" s

kilo_assign= "{lval_a[idx_a, idx_b:idx_c], lval_b} = " ++
	"r_val_a+r_val_b-r_val_c;"
mega_statement = "{lval_a[2, 12:16], lval_b} = " ++
	"r_val_a+r_val_b-r_val_c"