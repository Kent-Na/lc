{-# LANGUAGE FlexibleContexts #-}
module Parse (
    Type(Unresolved_type, Array_type),

    Identifier,
    Object(Undefined_object, Module, Value_object),

    Bit_width,
    Bit_value,

    Instance_mod(In, Out, Reg, Latch),
    Statement(Define, Instantiate, Assign, Return, If_chain, On),

    Slice_range(Slice_range, Slice_point),

    Expr(
        Bit_array_value,
        Meta_number_value,
        Meta_string_value,
        Id_value,
        Unaly_operator,
        Binaly_operator,
        Field_access,
        Slice_operator,
        Cat_operator,
        Function_call,
        Undefined_value),
    
    dummy_pos,
    pos_of,
    parse_str,

    lc_assign,
    lc_variable_define,
    parse_str_with
)where

import Token 
import Text.Parsec
import Text.Parsec.Pos (newPos)
import Text.Parsec.String (GenParser)
import Text.Parsec.String (Parser)

import Data.List (intercalate)
import Data.Char
import Control.Monad
import Numeric 

import Data.Map (Map)
import qualified Data.Map

-----------
-- Primitive token parsers

type Token_parser = Parsec [(SourcePos, Token)] () 

any_token :: Token_parser Token
any_token = satisfy_token (const True)

is_user_token :: Token -> Bool
is_user_token (User_token s) = True
is_user_token _ = False 

user_token = (satisfy_token is_user_token) <?> "user_token"

is_literal_token :: Token -> Bool
is_literal_token (Bit_array_literal _ _) = True
is_literal_token (Meta_number_literal _ _) = True
is_literal_token _ = False 

literal_token = satisfy_token is_literal_token <?> "literal_token"

is_predefined_token :: Token -> Bool
is_predefined_token (Predefined_token _) = True
is_predefined_token _ = False 

predefined_token = 
    satisfy_token is_predefined_token <?> "predefined_token"

satisfy_token :: (Token -> Bool) -> (Token_parser Token)
satisfy_token f = 
    tokenPrim (\(pos, tok) -> show tok)
              (\pos x xs -> update_pos_token xs)
              (\(pos, tok) -> if f tok then Just tok else Nothing)
    where 
        update_pos_token ((x,_):_) = x
        update_pos_token [] = newPos "!endoffile!" 0 0 

--String token. Parses a token that mach with given string.
s_token :: String -> Token_parser Token
s_token s = 
    f (parse lc_token "" s)
    where 
        f (Right (pos, tok)) = satisfy_token (== tok) <?> s
        f (Left _ ) = unexpected ("bad token (bug in compiler) " ++ s)

--Core data structures

type Unresolved_id = String

type Identifier = String

data Object 
    =Undefined_object
    |Module [Statement]
    |Value_object Expr
    deriving(Show)

data Type
    =Unresolved_type Identifier 
    |Array_type Type Expr
    deriving(Show)

type Bit_width = Int
type Bit_value = Int

data Instance_mod = In | Out | Reg | Latch
    deriving(Show, Eq)

data Statement
    =Define SourcePos Identifier Object
    |Instantiate SourcePos Type Identifier [Instance_mod]
    -- a = b
    --Left hand Expr must be assinable
    --Assign lvalue rvalue
    |Assign SourcePos Expr Expr
    |Return SourcePos Expr 
    -- If ifel else block.
    -- condition internal_statement chained_condition_block
    -- else block have always true condition.
    |If_chain [(Expr, [Statement])]
    -- On block
    -- timing pre_condition post_condition internal_statements
    |On Expr Expr Expr [Statement]
    |Undefined_statement
    deriving(Show)

data Slice_range
    =Slice_range Expr Expr 
    |Slice_point Expr
    deriving(Show)

data Expr 
    =Bit_array_value    SourcePos Bit_width Bit_value
    |Meta_number_value  SourcePos Int Unit
    |Meta_string_value  SourcePos String
    |Id_value           SourcePos Identifier 
    |Unaly_operator     SourcePos String Expr
    |Binaly_operator    SourcePos String Expr Expr
    |Field_access       SourcePos Expr Identifier 
    |Slice_operator     SourcePos Expr [Slice_range]
    |Cat_operator       SourcePos [Expr]
    |Function_call      SourcePos Identifier [Expr]
    |Undefined_value
    deriving(Show)

dummy_pos::SourcePos
dummy_pos = newPos "<dummy>" 0 0

class Parse_element e where
    pos_of :: e -> SourcePos

instance Parse_element Statement where
    pos_of (Define pos _ _) = pos
    pos_of (Instantiate pos _ _ _) = pos
    pos_of (Assign pos _ _) = pos
    pos_of (Return pos _) = pos

----
--Token parsers

lc_parse_file::Token_parser [Statement]
lc_parse_file = do
    stmts <- many $ lc_top_level
    eof
    return $ concat stmts

lc_top_level::Token_parser [Statement]
lc_top_level = do
    stmts <- choice [
        try lc_import,
        lc_define]
    return $ stmts

--import x;
lc_import::Token_parser [Statement]
lc_import = do
    s_token "import"
    user_token
    s_token ";"
    return $ (:[]) Undefined_statement

--x:y
lc_define::Token_parser [Statement] 
lc_define = do
    stmt <- choice [try lc_function_define, 
        try lc_module_define, 
        try lc_struct_define]
    return [stmt]

lc_function_define = 
    lc_define_base lc_func_block 

lc_module_define = 
    lc_define_base lc_module_block 

lc_struct_define = 
    lc_define_base lc_struct_block 

lc_define_base::Token_parser Object
              ->Token_parser Statement
lc_define_base p = do
    pos <- getPosition
    User_token name <- user_token
    s_token ":"
    obj <- p 
    return $ Define pos name obj

-------
--Blocks

--func a -> b -> ... -> x { ... }
lc_func_block:: Token_parser Object 
lc_func_block = do
    s_token "func"
    args <- sepBy lc_func_arg (s_token "->")
    
    s_token "{"
    statements <- many $ choice [
        try lc_assign, 
        lc_return]
    s_token "}"

    return $ Undefined_object 

lc_func_arg  = do
    var_type <- lc_type
    var_name <- user_token 
    return (var_type, var_name)

--struct { ... }
lc_struct_block:: Token_parser Object 
lc_struct_block = do
    s_token "struct"

    s_token "{"
    statements <- many $ lc_variable_define
    s_token "}"

    return $ Undefined_object

--module { ... }
lc_module_block:: Token_parser Object 
lc_module_block = do
    s_token "module"

    s_token "{"
    stmts <- many $ choice [
        try lc_on_block,
        try lc_define,
        try lc_assign,
        try lc_variable_define]
    s_token "}"

    return $ Module (concat stmts)

--a on b -> c { ... }
lc_on_block:: Token_parser [Statement]
lc_on_block = do
    tgt <- lc_expression 100
    s_token "on"
    s_token "posedge"

    s_token "{"
    stmts <- many $ choice [
        try lc_if_elif_else_block,
        try lc_assign,
        try lc_incliment]
    s_token "}"
    return $ (:[]) $ On 
        tgt 
        (Bit_array_value dummy_pos 1 0) 
        (Bit_array_value dummy_pos 1 1) 
        (concat stmts)

--if a {} elif b {} else {}
lc_if_elif_else_block:: Token_parser [Statement]
lc_if_elif_else_block = do
    x <- lc_if_block
    xs <- many lc_elif_block
    xops <- optionMaybe lc_else_block
    return $ (:[]) $
        If_chain (result x xs xops)
    where 
        result x xs (Just xops)= (x:xs)++[xops]
        result x xs (Nothing)= x:xs

--if a {}
lc_if_block = lc_if_elif_block "if"

--elif a {}
lc_elif_block = lc_if_elif_block "elif"

lc_if_elif_block:: String -> Token_parser (Expr, [Statement])
lc_if_elif_block label = do
    s_token label
    cond <- lc_expression 100
    s_token "{"
    stmts <- many $ choice [
        try lc_if_elif_else_block,
        try lc_assign,
        try lc_incliment]
    s_token "}"
    return (cond, concat stmts)

--else {}
lc_else_block:: Token_parser (Expr, [Statement])
lc_else_block = do
    s_token "else"
    s_token "{"
    stmts <- many $ choice [
        try lc_if_elif_else_block,
        try lc_assign,
        try lc_incliment]
    s_token "}"
    return (Bit_array_value dummy_pos 1 1, concat stmts)


-- "reg" T a;
lc_instance_mod::Token_parser Instance_mod
lc_instance_mod = do
    mod <- choice [
        s_token "in", s_token "out",
        s_token "reg", s_token "latch"]
    case mod of
        Keyword "in"    -> return In
        Keyword "out"   -> return Out
        Keyword "reg"   -> return Reg
        Keyword "latch" -> return Latch

-- T a;
lc_variable_define::Token_parser [Statement]
lc_variable_define = do
    pos <- getPosition
    mods <- many lc_instance_mod
    var_type <- lc_type
    User_token var_name <- user_token 
    option [] (lc_variable_init var_name)
    s_token ";"
    return $ (:[]) $ Instantiate pos var_type var_name mods

-- T a "= a"
lc_variable_init::String -> Token_parser [Statement]
lc_variable_init id = do
    pos_eq <- getPosition
    s_token "="
    pos_val <- getPosition
    r <- lc_expression 100
    return $ (:[]) $ Assign pos_eq (Id_value dummy_pos id) r

-- T
lc_base_type:: Token_parser Type
lc_base_type = do
    t <- try user_token <|> try predefined_token <?> "type name"
    case t of 
        User_token       name -> return $ Unresolved_type name
        Predefined_token name -> return $ Unresolved_type name
-- T[a]
lc_type:: Token_parser Type 
lc_type= do
    base <- lc_base_type 
    array_args <- many lc_array_type_decolator
    return $ foldl Array_type base array_args

-- part [a] of T[a] 
lc_array_type_decolator:: Token_parser Expr
lc_array_type_decolator = do
    s_token "["
    s <- lc_expression 100
    s_token "]"
    return s

-- return a;
lc_return :: Token_parser [Statement]
lc_return = do
    pos <- getPosition
    s_token "return"
    v <- lc_expression 100
    s_token ";"
    return $ (:[]) $ Return pos v

-- a = b;
lc_assign::Token_parser [Statement]
lc_assign = do 
    l <- lc_expression 100
    pos <- getPosition
    s_token "="
    r <- lc_expression 100
    s_token ";"
    return $ (:[]) $ Assign pos l r

-- a ++;
lc_incliment:: Token_parser [Statement]
lc_incliment = do
    v <- lc_expression 100
    pos <- getPosition
    s_token "++"
    s_token ";"
    return $ (:[]) $ Assign pos v 
            (Binaly_operator dummy_pos "+" v 
                (Bit_array_value dummy_pos 1 1))

----
--Expression parsers

type P_level = Int

require :: Bool -> Token_parser ()
require cond = req cond $ return ()

req :: Bool -> Token_parser a -> Token_parser a
req cond base = 
    if cond then try base else parserZero

-- a
lc_expression:: P_level -> Token_parser Expr
lc_expression lv = do
    expr <- lc_expression_with_prefix lv
    option expr (lc_expression_postfix lv expr)

-- a "+ b"
lc_expression_postfix:: P_level ->Expr -> Token_parser Expr
lc_expression_postfix lv v = do
    v' <- choice[
        req (lv>1) $ lc_field_access v,
        req (lv>3) $ lc_binary_op v 3 ["*", "/"],
        req (lv>4) $ lc_binary_op v 4 ["+", "-"],
        req (lv>5) $ lc_binary_op v 5 ["&"],
        req (lv>6) $ lc_binary_op v 6 ["|", "^"],
        req (lv>7) $ lc_binary_op v 7 ["<>"],
        req (lv>8) $ lc_binary_op v 8 ["==", "!=", "<", "<=", ">", ">="],
        req (lv>1) $ lc_slice_op v]
    option v' (lc_expression_postfix lv v')

--Statements with prefix operatores or/and braces.
lc_expression_with_prefix:: P_level -> Token_parser Expr
lc_expression_with_prefix lv = do
    choice [
        try $ lc_variable,
        try $ lc_literal,
        try $ between (s_token "(") (s_token ")") (lc_expression 100),
        req (lv>2) $ lc_unaly_op 2, 
        req (lv>0) $ lc_cat_op,
        req (lv>1) $ lc_function_call]

-- a
lc_variable:: Token_parser Expr
lc_variable = do
    pos <- getPosition
    User_token name <- user_token
    return $ Id_value pos name

-- f(a)
lc_function_call:: Token_parser Expr 
lc_function_call = do 
    pos <- getPosition
    User_token name <- user_token
    s_token "("
    params <- sepBy1 (lc_expression 100) (s_token ",")
    s_token ")"
    return $ Function_call pos name params

-- 1'b1
lc_literal:: Token_parser Expr
lc_literal = do
    pos <- getPosition
    v <- literal_token
    case v of
        (Bit_array_literal w v)   -> return $ Bit_array_value pos w v
        (Meta_number_literal v u) -> return $ Meta_number_value pos v u

----
-- -a
lc_unaly_op:: P_level -> Token_parser Expr
lc_unaly_op lv = do
    pos <- getPosition
    Symbol op <- unary_op 
    val <- lc_expression lv
    return $ Unaly_operator pos op val
	where
		unary_op :: Token_parser Token
		unary_op = choice (map s_token ["+", "-", "&", "|", "~"]) <?>
			"unary operator"

--a + b
lc_binary_op::Expr -> P_level -> [String] -> Token_parser Expr
lc_binary_op left lv ops = do
    pos <- getPosition
    Symbol op <- binary_op
    right <- lc_expression lv
	--_with_prefix
    return $ Binaly_operator pos op left right
	where
		binary_op :: Token_parser Token
		binary_op = choice (map s_token ops) <?> "binary operator"

--a . b
lc_field_access::Expr -> Token_parser Expr
lc_field_access base= do
    pos <- getPosition
    s_token "."
    User_token name <- user_token 
    return $ Field_access pos base name

--{a, b, c}
lc_cat_op:: Token_parser Expr
lc_cat_op = do 
    pos <- getPosition
    statements <- between (s_token "{") (s_token "}")
        (sepBy1 (lc_expression 100) (s_token ","))
    return $ Cat_operator pos statements

--a[b:c, d]
lc_slice_op:: Expr -> Token_parser Expr
lc_slice_op v = do
    pos <- getPosition
    ranges <- between (s_token "[") (s_token "]")
        (sepBy1 lc_slice_range (s_token ","))
    return $ Slice_operator pos v ranges

--Parse the part 'a:b' or 'c' of x[a:b,c]
lc_slice_range:: Token_parser Slice_range
lc_slice_range = choice [
    try lc_slice_range_d,
    try lc_slice_range_s]

--Parse the part 'c' of x[a:b,c]
lc_slice_range_s:: Token_parser Slice_range
lc_slice_range_s = do
    point <- lc_expression 100
    return $ Slice_point point 

--Parse the part 'a:b' of x[a:b,c]
lc_slice_range_d:: Token_parser Slice_range
lc_slice_range_d = do
    begin <- lc_expression 100
    s_token ":"
    end <- lc_expression 100
    return $ Slice_range begin end

----
--for debug
parse_str_with p s = 
    runParser p () "" (f (parse lc_tokenize_file "" s))
    where 
        f (Right s) = s
        f (Left _) = []
parse_str s = parse_str_with lc_parse_file s

kilo_assign= "{lval_a[idx_a, idx_b:idx_c], lval_b} = " ++
	"r_val_a+r_val_b-r_val_c;"
mega_assign= "{lval_a[2, 12:16], lval_b} = " ++
	"{r_val_a,r_val_b}[3]-r_val_c;"
mega_expr = "a*b|c+d==e&f+g"
