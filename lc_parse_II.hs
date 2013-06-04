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
lc_parse_file::Token_parser Token
lc_parse_file = do
    result <- many $ lc_top_level
    eof
    return $ User_token "file"

lc_top_level::Token_parser Token
lc_top_level = do
    choice [
        try lc_import,
        lc_define]
    return $ User_token "top_level"

--import x;
lc_import::Token_parser Token
lc_import = do
    s_token "import"
    user_token
    s_token ";"
    return $ User_token "import"

--x:y
lc_define::Token_parser Token
lc_define = do
    user_token
    s_token ":"
    choice[
        try lc_func_block,
        try lc_module_block,
        lc_struct_block]
    return $ User_token "define"

-------
--Blocks

--func a -> b -> ... -> x { ... }
lc_func_block:: Token_parser Token 
lc_func_block = do
    s_token "func"
    args <- sepBy (type_name_token <+> user_token) (s_token "->")
    
    s_token "{"
    statements <- many $ choice [
        try lc_assign, 
        try lc_incliment, 
        lc_return]
    s_token "}"

    return $ User_token "func"

--struct { ... }
lc_struct_block:: Token_parser Token 
lc_struct_block = do
    s_token "struct"

    s_token "{"
    statements <- many $ lc_variable_define
    s_token "}"

    return $ User_token "func"

--module { ... }
lc_module_block:: Token_parser Token 
lc_module_block = do
    s_token "module"

    s_token "{"
    statements <- many $ choice [
        try lc_on_block,
        try lc_define,
        try lc_port_define,
        try lc_reg_define,
        try lc_assign,
        try lc_variable_define]
    s_token "}"

    return $ User_token "module"

--a on b -> c { ... }
lc_on_block:: Token_parser Token 
lc_on_block = do
    user_token
    s_token "on"
    s_token "posedge"

    s_token "{"
    statements <- many $ choice [
        try lc_if_elif_else_block,
        try lc_assign,
        try lc_incliment]
    s_token "}"

    return $ User_token "on"

lc_if_elif_else_block:: Token_parser Token
lc_if_elif_else_block = do
    lc_if_block
    many lc_elif_block
    option (User_token "dummy") lc_else_block
    return $ User_token "if-elif-else"

--if a {}
lc_if_block:: Token_parser Token 
lc_if_block = do
    s_token "if"
    lc_statement

    s_token "{"
    statements <- many $ choice [
        try lc_if_elif_else_block,
        try lc_assign,
        try lc_incliment]
    s_token "}"

    return $ User_token "if"

--elif a {}
lc_elif_block:: Token_parser Token 
lc_elif_block = do
    s_token "elif"
    lc_statement

    s_token "{"
    statements <- many $ choice [
        try lc_if_elif_else_block,
        try lc_assign,
        try lc_incliment]
    s_token "}"

    return $ User_token "elif"

--else {}
lc_else_block:: Token_parser Token 
lc_else_block = do
    s_token "else"

    s_token "{"
    statements <- many $ choice [
        try lc_if_elif_else_block,
        try lc_assign,
        try lc_incliment]
    s_token "}"

    return $ User_token "else"


lc_port_define::Token_parser Token
lc_port_define= do
    choice [s_token "in", s_token "out"]
    lc_variable_define

lc_reg_define::Token_parser Token
lc_reg_define= do
    s_token "reg"
    lc_variable_define

lc_variable_define::Token_parser Token
lc_variable_define = do
    var_type <- lc_type_name 
    var_name <- user_token 
    option (User_token "dummy") lc_variable_init
    s_token ";"
    return $ User_token "variable_def"

lc_variable_init = do
    s_token "="
    lc_statement

lc_type_name = do
    type_name_token
    option (User_token "dummy") (do{
        s_token "[";
        literal_token;
        s_token "]";
        } )

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

lc_incliment = do
    lc_statement
    s_token "++"
    s_token ";"
    return $ User_token "++"


lc_function_call:: Token_parser Token
lc_function_call = do 
    user_token
    s_token "("
    sepBy1 lc_statement (s_token ",")
    s_token ")"

lc_n_statement:: Token_parser Token
lc_n_statement = do
    choice [
        try (unary_op<+>lc_statement), 
        try $ between (s_token "(") (s_token ")") lc_statement,
        try lc_cat_op,
        try lc_function_call,
        try user_token,
        try literal_token]
    return $ User_token "n_statement"

lc_statement :: Token_parser Token 
lc_statement = do
    lc_n_statement
    many $ choice [
        binary_op<+> lc_n_statement,
        lc_slice_op]
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

parse_str_with p s = parse p "" (f (parse lc_tokenize_file "" s))
    where 
        f (Right s) = s
        f (Left _) = []
parse_str s = parse_str_with lc_parse_file s

--for debug
kilo_assign= "{lval_a[idx_a, idx_b:idx_c], lval_b} = " ++
	"r_val_a+r_val_b-r_val_c;"
mega_assign= "{lval_a[2, 12:16], lval_b} = " ++
	"{r_val_a,r_val_b}[3]-r_val_c;"

--parse_lc :: String -> Either ParseError String 
--parse_lc input = parse lc_file "" input
