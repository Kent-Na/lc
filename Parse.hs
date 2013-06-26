{-# LANGUAGE FlexibleContexts #-}
module Parse (
    Type(Unresolved_type, Array_type),

    Identifier,
    Object(Undefined_object, Module, Instance_object, Value_object),

    Bit_width,
    Bit_value,

    Statement(Define, Assign, Return, If_chain, On),

    Slice_range(Slice_range, Slice_point),

    Value(
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

    parse_str,

    lc_assign,
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
-- primitive parsers

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

type_name_token :: Token_parser Token
type_name_token = try user_token <|> try predefined_token <?> "type name"

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
        f (Left _ ) = unexpected "bad token (bug in compiler)"

unary_op :: Token_parser Token
unary_op = choice (map s_token ["+", "-", "&", "|", "~"]) <?>
    "unary operator"

binary_op :: Token_parser Token
binary_op = choice (map s_token 
    ["+", "-", "^", "&", "|", "==", ">>"]) <?>
    "binary operator"

----
--core data structures


type Unresolved_id = String

type Identifier = String

data Object 
    =Undefined_object
    |Module [Statement]
    -- Created by the instance define statement.
    -- Parameter is a idintifier of type.
    |Instance_object Type 
    |Value_object Value
    deriving(Show)

data Type
    =Unresolved_type Identifier 
    |Array_type Type Value
    deriving(Show)

type Bit_width = Int
type Bit_value = Int

-- a = b
--Left hand value must be assinable
--Assign lvalue rvalue
data Statement
    =Define Identifier Object
    |Instantiate Type Identifier
    |Assign SourcePos Value Value
    |Return SourcePos Value 
    -- If ifel else block.
    -- condition internal_statement chained_condition_block
    -- else block have always true condition.
    |If_chain [(Value, [Statement])]
    -- On block
    -- timing pre_condition post_condition internal_statements
    |On Value Value Value [Statement]
    |Undefined_statement
    deriving(Show)

data Slice_range
    =Slice_range Value Value 
    |Slice_point Value
    deriving(Show)

data Value 
    =Bit_array_value    SourcePos Bit_width Bit_value
    |Meta_number_value  SourcePos Int Unit
    |Meta_string_value  SourcePos String
    |Id_value           SourcePos Identifier 
    |Unaly_operator     SourcePos String Value
    |Binaly_operator    SourcePos String Value Value
    |Field_access       SourcePos Value Identifier 
    |Slice_operator     SourcePos Value [Slice_range]
    |Cat_operator       SourcePos [Value]
    |Function_call      SourcePos Identifier [Value]
    |Undefined_value
    deriving(Show)

dummy_pos::SourcePos
dummy_pos = newPos "<dummy>" 0 0

----
--core parse states
{-
    type Parse_state = [Scope]

data Scope 
    = Scope 
        (Map Identifier Object) 
        --(Map String Variable)
        [Statement]
    deriving(Show)

brank_state = [brank_scope] 
brank_scope = Scope Data.Map.empty []

push_state:: Token_parser()
push_state = modifyState (\s -> brank_scope:s)

pop_state:: Token_parser Scope
pop_state = do
    s <- getState
    modifyState tail
    return $ head s

add_object::Identifier -> Object -> Token_parser ()
add_object s d= do
    st <- getState
    if member_of_scope (head st)
        then fail $ "Duplicative definition of " ++ s
        else setState $ add_to_scope (head st) : tail st
    where 
        member_of_scope (Scope ds stmt)
            = Data.Map.member s ds
        add_to_scope (Scope ds stmt) 
            = Scope (Data.Map.insert s d ds) stmt
    
add_statement::Statement -> Token_parser ()
add_statement s = do
    st <- getState
    setState $ add_to_scope (head st) : tail st
    where 
        add_to_scope (Scope ds stmt) 
            = Scope ds (s:stmt)
-}

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

lc_define_base::(Token_parser Object) 
              ->Token_parser Statement
lc_define_base p = do
    name <- user_token
    s_token ":"
    obj <- p 
    return $ Define (value name) obj

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
        try lc_port_define,
        try lc_reg_define,
        try lc_assign,
        try lc_variable_define]
    s_token "}"

    return $ Module (concat stmts)

--a on b -> c { ... }
lc_on_block:: Token_parser [Statement]
lc_on_block = do
    tgt <- lc_value 
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

lc_if_elif_block:: String -> Token_parser (Value, [Statement])
lc_if_elif_block label = do
    s_token label
    cond <- lc_value
    s_token "{"
    stmts <- many $ choice [
        try lc_if_elif_else_block,
        try lc_assign,
        try lc_incliment]
    s_token "}"
    return (cond, concat stmts)

--else {}
lc_else_block:: Token_parser (Value, [Statement])
lc_else_block = do
    s_token "else"
    s_token "{"
    stmts <- many $ choice [
        try lc_if_elif_else_block,
        try lc_assign,
        try lc_incliment]
    s_token "}"
    return (Bit_array_value dummy_pos 1 1, concat stmts)

-- in T a;
lc_port_define::Token_parser [Statement]
lc_port_define= do
    choice [s_token "in", s_token "out"]
    lc_variable_define

-- reg T a;
lc_reg_define::Token_parser [Statement]
lc_reg_define= do
    s_token "reg"
    lc_variable_define

-- T a;
lc_variable_define::Token_parser [Statement]
lc_variable_define = do
    var_type <- lc_type_name 
    var_name <- user_token 
    --option (Meta_string_value "dummy") 
    optional lc_variable_init
    s_token ";"
    return $ (:[]) $ Instantiate var_type (value var_name)

-- T a "= a"
lc_variable_init = do
    s_token "="
    lc_value

lc_func_arg= do
    var_type <- lc_type_name 
    var_name <- user_token 
    return (var_type, var_name)
-- T
-- T[a]
lc_type_name:: Token_parser Type 
lc_type_name = do
    base <- type_name_token
    array_args <- many lc_array_type_decolator
    return $ foldl Array_type (Unresolved_type (value base)) array_args

-- part [a] of T[a] 
lc_array_type_decolator:: Token_parser Value
lc_array_type_decolator = do
    s_token "[";
    s <- lc_value;
    s_token "]";
    return s

-- return a;
lc_return :: Token_parser [Statement]
lc_return = do
    pos <- getPosition
    s_token "return"
    v <- lc_value
    s_token ";"
    return $ (:[]) $ Return pos v

-- a = b;
lc_assign::Token_parser [Statement]
lc_assign = do 
    l <- lc_value
    pos <- getPosition
    s_token "="
    r <- lc_value
    s_token ";"
    return $ (:[]) $ Assign pos l r

-- a ++;
lc_incliment:: Token_parser [Statement]
lc_incliment = do
    v <- lc_value
    pos <- getPosition
    s_token "++"
    s_token ";"
    return $ (:[]) $ Assign pos v 
            (Binaly_operator dummy_pos "+" v 
                (Bit_array_value dummy_pos 1 1))

----
--Value parsers

lc_function_call:: Token_parser Value 
lc_function_call = do 
    pos <- getPosition
    name <- user_token
    s_token "("
    params <- sepBy1 lc_value (s_token ",")
    s_token ")"
    return $ Function_call pos (value name) params

lc_value:: Token_parser Value
lc_value= do
    value <- lc_value_with_prefix
    option value (lc_value_postfix value)

lc_value_postfix:: Value -> Token_parser Value
lc_value_postfix v = do
    v' <- choice[
        lc_binary_op v,
        lc_field_access v,
        lc_slice_op v]
    option v' (lc_value_postfix v')

--Statements with prefix operatores or/and braces.
lc_value_with_prefix:: Token_parser Value
lc_value_with_prefix = do
    choice [
        try lc_unaly_op, 
        try $ between (s_token "(") (s_token ")") lc_value,
        try lc_cat_op,
        try lc_function_call,
        try lc_variable,
        try lc_literal]

lc_variable:: Token_parser Value
lc_variable = do
    pos <- getPosition
    name <- user_token
    return $ Id_value pos (value name)

lc_literal:: Token_parser Value
lc_literal = do
    pos <- getPosition
    v <- literal_token
    case v of
        (Bit_array_literal w v)   -> return $ Bit_array_value pos w v
        (Meta_number_literal v u) -> return $ Meta_number_value pos v u

-- -a
lc_unaly_op::Token_parser Value
lc_unaly_op = do
    pos <- getPosition
    op <- unary_op 
    val <- lc_value
    return $ Unaly_operator pos (value op) val

--a + b
lc_binary_op::Value -> Token_parser Value
lc_binary_op left = do
    pos <- getPosition
    op <- binary_op
    right <- lc_value_with_prefix
    return $ Binaly_operator pos (value op) left right

--a . b
lc_field_access::Value -> Token_parser Value
lc_field_access base= do
    pos <- getPosition
    s_token "."
    name <- lc_field_name
    return $ Field_access pos base name

lc_field_name::Token_parser String
lc_field_name= do
    name <- user_token
    return $ value name

--{a, b, c}
lc_cat_op:: Token_parser Value
lc_cat_op = do 
    pos <- getPosition
    statements <- between (s_token "{") (s_token "}")
        (sepBy1 lc_value (s_token ","))
    return $ Cat_operator pos statements

--a[b:c, d]
lc_slice_op:: Value -> Token_parser Value
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
    point <- lc_value
    return $ Slice_point point 

--Parse the part 'a:b' of x[a:b,c]
lc_slice_range_d:: Token_parser Slice_range
lc_slice_range_d = do
    begin <- lc_value
    s_token ":"
    end <- lc_value
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
