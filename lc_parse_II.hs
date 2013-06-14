{-# LANGUAGE FlexibleContexts #-}
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

type Token_parser = Parsec [(SourcePos, Token)] Parse_state

is_user_token :: Token -> Bool
is_user_token (User_token s) = True
is_user_token _ = False 

user_token = (satisfy_token is_user_token) <?> "user_token"

is_literal_token :: Token -> Bool
is_literal_token (Bit_array_literal _ _) = True
is_literal_token (Meta_number_literal _ _) = True
is_literal_token _ = False 

literal_token = satisfy_token is_literal_token <?> "literal_token"

update_pos_token ((x,_):_) = x
update_pos_token [] = newPos "!endoffile!" 0 0 

satisfy_token :: (Token -> Bool) -> (Token_parser Token)
satisfy_token f = 
    tokenPrim (\(pos, tok) -> show tok)
              (\pos x xs -> update_pos_token xs)
              (\(pos, tok) -> if f tok then Just tok else Nothing)

any_token :: Token_parser Token
any_token = satisfy_token (const True)

t_token tok = satisfy_token (== tok)


is_predefined_token :: Token -> Bool
is_predefined_token (Predefined_token _) = True
is_predefined_token _ = False 

predefined_token = satisfy_token is_predefined_token <?> "predefined_token"

type_name_token :: Token_parser Token
type_name_token = try user_token <|> try predefined_token

--String token. Parses a token that mach with given string.
s_token :: String -> Token_parser Token
s_token s = 
   f x
   where 
    f (Right (pos, tok)) = satisfy_token (== tok) <?> s
    f (Left _ ) = unexpected "bad token (bug in compiler)"
    x = (parse lc_token "" s)

unary_op :: Token_parser Token
unary_op = choice (map s_token ["+", "-", "&", "|"]) <?>
    "unary operator"

binary_op :: Token_parser Token
binary_op = choice (map s_token 
    ["+", "-", "^", "&", "|", "==", ".", ">>"]) <?>
    "binary operator"

(<:>) :: (Token_parser a) -> (Token_parser [a]) -> (Token_parser [a])
l <:> r = liftM2 (:) l r
infixr 5 <:>

(<+>) :: (Token_parser a) -> (Token_parser b) -> (Token_parser b)
l <+> r = do
    l
    r

----
--core data structures
type Type_name = String 
data Type
    =Unresolved_type Type_name
    |Array_type Type Value
    deriving(Show)

type Variable_name = String 

data Variable
    =Variable Type 
    |Unresolved_variable Variable_name
    deriving(Show)

type Clock = Value
data Register = Register Type Clock Variable_name

type Definition_name = String

data Definition 
    =Function_definition Function
    |Struct_definition Struct
    |Module_definition Module
    deriving(Show)

data Function
    =Unresolved_function Definition_name 
    |Undefined_function
    |Function [Variable] Scope
    deriving(Show)

data Struct 
    =Unresolved_struct Definition_name 
    |Undefined_struct
    |Struct Scope
    deriving(Show)

data Module 
    =Unresolved_Module Definition_name 
    |Undefined_module
    |Module Scope
    deriving(Show)


type Bit_width = Int
type Bit_value = Int

-- a = b
--Left hand value must be assinable
data Assign = Assign Value Value

data Reg_assign = Reg_assign Value Value

data Slice_range
    =Slice_range Value Value 
    |Slice_point Value
    deriving(Show)

data Value 
    =Bit_array_value Bit_width Bit_value
    |Meta_number_value Int Unit
    |Meta_string_value String
    |Variable_value Variable
    |Unaly_operator String Value
    |Binaly_operator String Value Value
    |Slice_operator Value [Slice_range]
    |Cat_operator [Value]
    |Function_call Function [Value]
    |Undefined_value
    deriving(Show)

----
--core parse states

type Parse_state = [Scope]

data Scope 
    = Scope 
        (Map String Definition) 
        (Map String Variable)
    deriving(Show)

brank_state = [brank_scope] 
brank_scope = Scope Data.Map.empty Data.Map.empty

push_state:: Token_parser()
push_state = modifyState (\s -> brank_scope:s)

pop_state:: Token_parser Scope
pop_state = do
    s <- getState
    modifyState tail
    return $ head s

add_definition::String -> Definition -> Token_parser ()
add_definition s d= do
    st <- getState
    setState $ add_to_scope (head st) : tail st
    where 
        add_to_scope (Scope ds vs) = Scope (Data.Map.insert s d ds) vs
    
add_variable::String -> Variable -> Token_parser ()
add_variable s v= do
    st <- getState
    setState $ add_to_scope (head st) : tail st
    where 
        add_to_scope (Scope ds vs) = Scope ds (Data.Map.insert s v vs)

--is_const::Value -> Bool
--is_assignable::Value -> Bool

----
--Token parsers

lc_parse_file::Token_parser Parse_state
lc_parse_file = do
    result <- many $ lc_top_level
    eof
    getState

lc_top_level::Token_parser Token
lc_top_level = do
    choice [
        try lc_import,
        lc_define]
    return $ User_token "top_level"

--import x;
lc_import::Token_parser () 
lc_import = do
    s_token "import"
    user_token
    s_token ";"
    return ()

--x:y
lc_define::Token_parser () 
lc_define = do
    choice [try lc_function_define, 
        try lc_module_define, 
        try lc_struct_define]
    return ()

lc_function_define = 
    lc_define_base lc_func_block Function_definition

lc_module_define = 
    lc_define_base lc_module_block Module_definition

lc_struct_define = 
    lc_define_base lc_struct_block Struct_definition

lc_define_base::(Token_parser a) 
    ->(a -> Definition) 
    ->Token_parser ()
lc_define_base p d= do
    name <- user_token
    s_token ":"
    f <- p 
    add_definition (value name) (d f)
    --return $ d (value name) f
    


-------
--Blocks

--func a -> b -> ... -> x { ... }
lc_func_block:: Token_parser Function 
lc_func_block = do
    s_token "func"
    args <- sepBy (type_name_token <+> user_token) (s_token "->")
    
    s_token "{"
    statements <- many $ choice [
        try lc_assign, 
        try lc_incliment, 
        lc_return]
    s_token "}"

    return $ Undefined_function 

--struct { ... }
lc_struct_block:: Token_parser Struct 
lc_struct_block = do
    s_token "struct"

    s_token "{"
    statements <- many $ lc_variable_define
    s_token "}"

    return $ Undefined_struct

--module { ... }
lc_module_block:: Token_parser Module
lc_module_block = do
    push_state
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
    st <- pop_state

    return $ Module st

--a on b -> c { ... }
lc_on_block:: Token_parser () 
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
    
    return ()

lc_if_elif_else_block:: Token_parser ()
lc_if_elif_else_block = do
    lc_if_block
    many lc_elif_block
    option (User_token "dummy") lc_else_block
    return ()

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


-- in T a;
lc_port_define::Token_parser ()
lc_port_define= do
    choice [s_token "in", s_token "out"]
    lc_variable_define

-- reg T a;
lc_reg_define::Token_parser () 
lc_reg_define= do
    s_token "reg"
    lc_variable_define

-- T a;
lc_variable_define::Token_parser () 
lc_variable_define = do
    var_type <- lc_type_name 
    var_name <- user_token 
    option (Meta_string_value "dummy") lc_variable_init
    s_token ";"
    add_variable 
        (value var_name) 
        (Variable var_type)
    return ()

-- T a "= a"
lc_variable_init = do
    s_token "="
    lc_statement

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
    s <- lc_statement;
    s_token "]";
    return s

-- return a;
lc_return :: Token_parser ()
lc_return = do
    s_token "return"
    lc_statement
    s_token ";"
    return ()

-- a = b;
lc_assign::Token_parser () 
lc_assign = do 
    l <- lc_statement
    s_token "="
    r <- lc_statement
    s_token ";"
    return ()

-- a ++
lc_incliment = do
    lc_statement
    s_token "++"
    s_token ";"
    return ()


lc_function_call:: Token_parser Value 
lc_function_call = do 
    name <- user_token
    s_token "("
    params <- sepBy1 lc_statement (s_token ",")
    s_token ")"
    return $ Function_call (Unresolved_function (value name)) params

lc_statement:: Token_parser Value
lc_statement= do
    value <- lc_statement_pre
    option value (lc_statement_post value)

lc_statement_post:: Value -> Token_parser Value
lc_statement_post v = do
    v' <- choice[
        lc_binary_op v,
        lc_slice_op v]
    option v' (lc_statement_post v')

--Statements with prefix operatores or/and braces.
lc_statement_pre:: Token_parser Value
lc_statement_pre = do
    choice [
        try (unary_op<+>lc_statement), 
        try $ between (s_token "(") (s_token ")") lc_statement,
        try lc_cat_op,
        try lc_function_call,
        try lc_variable,
        try lc_literal]
    --return $ User_token "n_statement"

lc_variable:: Token_parser Value
lc_variable = do
    name <- user_token
    return $ Variable_value (Unresolved_variable (value name))

lc_literal:: Token_parser Value
lc_literal = do
    v <- literal_token
    return $ f v
    where 
        f (Bit_array_literal w v) = Bit_array_value w v
        f (Meta_number_literal v u) = Meta_number_value v u

lc_binary_op::Value -> Token_parser Value
lc_binary_op left = do
    op <- binary_op
    right <- lc_statement_pre
    return $ Binaly_operator (value op) left right

--{a, b, c}
lc_cat_op:: Token_parser Value
lc_cat_op = do 
    statements <- between (s_token "{") (s_token "}")
        (sepBy1 lc_statement (s_token ","))
    return $ Cat_operator statements

--a[b:c, d]
lc_slice_op:: Value -> Token_parser Value
lc_slice_op v = do
    ranges <- between (s_token "[") (s_token "]")
        (sepBy1 lc_slice_range (s_token ","))
    return $ Slice_operator v ranges

lc_slice_range:: Token_parser Slice_range
lc_slice_range = choice [
    try lc_slice_range_d,
    try lc_slice_range_s]

--Parse the part 'c' of x[a:b,c]
lc_slice_range_s:: Token_parser Slice_range
lc_slice_range_s = do
    point <- lc_statement
    return $ Slice_point point 

--Parse the part 'a:b' of x[a:b,c]
lc_slice_range_d:: Token_parser Slice_range
lc_slice_range_d = do
    begin <- lc_statement
    s_token ":"
    end <- lc_statement
    return $ Slice_range begin end


parse_str_with p s = 
    runParser p brank_state "" (f (parse lc_tokenize_file "" s))
    where 
        f (Right s) = s
        f (Left _) = []
parse_str s = parse_str_with lc_parse_file s

--for debug
kilo_assign= "{lval_a[idx_a, idx_b:idx_c], lval_b} = " ++
	"r_val_a+r_val_b-r_val_c;"
mega_assign= "{lval_a[2, 12:16], lval_b} = " ++
	"{r_val_a,r_val_b}[3]-r_val_c;"
