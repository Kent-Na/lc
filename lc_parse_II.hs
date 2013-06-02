{-# LANGUAGE FlexibleContexts #-}
import Token 
import Text.Parsec
import Text.Parsec.String (GenParser)
import Text.Parsec.String (Parser)

import Data.List (intercalate)
import Data.Char
import Control.Monad
import Numeric 

data Parse_tree

--type ParserL = Parsec [Token] ()

-------
--Blocks

lc_func_block:: Token_parser Token 
lc_func_block = do
    s_token "func"
    args <- sepBy (type_name_token <+> user_token) (s_token "->")
    
    s_token "{"
    statements <- many $ choice [
        try lc_assign, 
        try lc_return]
    s_token "}"

    return $ User_token "func"

lc_struct_block:: Token_parser Token 
lc_struct_block = do
    s_token "struct"

    s_token "{"
    statements <- many $ lc_variable_define
    s_token "}"

    return $ User_token "func"

lc_variable_define::Token_parser Token
lc_variable_define = do
    var_type <- type_name_token
    var_name <- user_token 
    s_token ";"
    return $ User_token "variable_def"

-- return x;
lc_return :: Token_parser Token
lc_return = do
    s_token "return"
    lc_statement
    s_token ";"
    return $ User_token "return"

-- a = b;
lc_assign::Token_parser Token 
lc_assign = do 
    l <- lc_statement
    s_token "="
    r <- lc_statement
    s_token ";"
    return $ User_token "assign"

lc_statement :: Token_parser Token 
lc_statement = do
    choice [
        try (unary_op<+>lc_statement), 
        try lc_cat_op,
        try (user_token<+>lc_slice_op),
        try (between (s_token "(") (s_token ")") lc_statement),
        try (user_token<+>binary_op<+>lc_statement),
        try user_token]
    return $ User_token "statement"

--{a, b, c}
lc_cat_op:: Token_parser Token 
lc_cat_op = do 
    statements <- between (s_token "{") (s_token "}")
        (sepBy1 lc_statement (s_token ","))
    return $ User_token "meow"

--a[b:c, d]
lc_slice_op:: Token_parser Token 
lc_slice_op = do
    statements <- between (s_token "[") (s_token "]")
        (sepBy1 (choice [
                try (lc_statement <+> s_token ":" <+> lc_statement),
                try (lc_statement)]) 
            (s_token ","))
    return $ User_token "slice"

--lc_file :: Parser [String]
--lc_file = line_comment 
--lc_file = do
	--result <- many $ try block_comment <|> try line_comment <|>
		--try func_block
	--eof
	--return result

parse_str s = parse lc_func_block "" (f (parse lc_tokenize_file "" s))
    where 
        f (Right s) = s
        f (Left _) = []

--for debug
kilo_assign= "{lval_a[idx_a, idx_b:idx_c], lval_b} = " ++
	"r_val_a+r_val_b-r_val_c;"
mega_statement = "{lval_a[2, 12:16], lval_b} = " ++
	"r_val_a+r_val_b-r_val_c"

--parse_lc :: String -> Either ParseError String 
--parse_lc input = parse lc_file "" input
