{-# LANGUAGE FlexibleInstances #-}
module Verilog_complier where

import Parse
import qualified Data.Map
import Data.Map (Map)
import Data.Maybe
import Text.Parsec.Pos

--Verilog data structure
data Ve_scope = 
    Ve_scope
    deriving (Show)

--name
--tunnel_port
--port
--statement

data Ve_direction = Input | Output | Inout
    deriving (Show)

data Ve_port = Ve_port Ve_direction Identifier
    deriving (Show)

data Ve_module = 
    Ve_module Identifier Ve_port
    deriving (Show)

----
--Compiler Monad
type ID_table = Map Identifier Identifier
type Nest_depth = Int
type UID = Int
data C_error = C_error SourcePos String
    deriving (Show)
data C_state = 
    C_state {
        --lc_scope :: [Lc_scope],
        id_table :: ID_table,
        ve_scope :: [Ve_scope],
        next_uid :: UID,
        error_list :: [C_error],
        nest_depth :: Nest_depth,
        out :: String
        }
    deriving (Show)

newtype Compiler a = Compiler{
    exec_compile:: C_state -> (a, C_state)
}

instance Monad Compiler where
    Compiler x >>= f = 
        Compiler out_func
        where out_func s_0 = (exec_compile (f a)) s_1
                where (a, s_1) = x s_0

    Compiler x >> Compiler y = 
        Compiler out_func
        where out_func s_0 = y s_1
                where (_, s_1) = x s_0

    return x = Compiler (\s -> (x, s))

compile :: Compiler a -> IO String
compile c = do
    print (error_list st)
    return (out st)
    where
        initial_state = C_state Data.Map.empty [] 0 [] 0 ""
        (x, st) = (exec_compile c) initial_state

do_test = do
    case parse_str_with lc_variable_define "bit a = 1+2;" of
        (Right [stmt])  -> compile (module_block_statement stmt)
        (Left err) -> return $ show err
        --"//parse err"
    
do_test_pls = do
    s <- readFile "simple_test.lc"
    case parse_str s of
        (Right stmts)  -> compile (top_level stmts)
        (Left err) -> return $ show err
        --"//parse err"
    
get_state::Compiler C_state
get_state = Compiler (\s -> (s,s))

set_state::C_state -> Compiler ()
set_state s = Compiler (\_ -> ((), s))

mod_state::(C_state -> C_state) -> Compiler ()
mod_state f = do
    s <- get_state
    set_state (f s)

put_error:: SourcePos -> String -> Compiler ()
put_error pos desc =
    mod_state f
    where
        f st = st{error_list = err : (error_list st)}
        err = C_error pos desc
    
add_token::String -> Compiler()
add_token tok = 
    mod_state f
    where 
        f st = st{out = (out st) ++ " " ++ tok}

add_tokens::[String] -> Compiler()
add_tokens tok = do 
    mapM add_token tok
    return ()

gen_uid:: Compiler UID 
gen_uid = do
    st <- get_state
    mod_state f
    return (next_uid st)
    where 
        f st = st{next_uid = (next_uid st) + 1}

new_id_entry:: String -> Compiler ()
new_id_entry id = do
    uid <- gen_uid
    mod_state (f uid)
    where 
        new_id uid = "_lc" ++ (show uid) ++ "_" ++ id
        f uid st = st{
            id_table = Data.Map.insert id (new_id uid) (id_table st)}

solve_id:: String -> Compiler String
solve_id id = do
    st <- get_state
    return $ Data.Map.findWithDefault "_00_bad_id" id (id_table st)
    --Scope s
    --return "bad_id"

top_level:: [Statement] -> Compiler () 
top_level stmts = do
    top_level_defines $ filter f stmts
    where 
        f (Define _ _) = True
        f _ = False

top_level_defines:: [Statement] -> Compiler ()
top_level_defines stmts = do
    mapM top_level_statement stmts
    return ()
    
top_level_statement:: Statement -> Compiler ()
top_level_statement (Define id (Module stmts)) = do
    --add_tokens ["module", id, "(", ")", ";"]
    --mapM module_block_statement stmts
    --add_token "endmodule"
    compile_object id (Module stmts)
    return ()

compile_object:: Identifier -> Object -> Compiler ()
compile_object m_id (Module stmts) = do
    --mapM new_id_entry (Data.Map.keys defs)
    mapM new_id_entry (map id instances)
    add_tokens ["module", m_id, "("]
    intersperse module_io (add_token ",") ports 
    add_tokens [")", ";"]
    mapM module_block_statement stmts 
    add_token "endmodule"
    return ()
    where 
        is_port_def (Instantiate t id mods) = 
            any (\s -> s == In || s == Out) mods
        is_port_def _ = False
        ports = filter is_port_def stmts

        is_instantiate (Instantiate _ _ _) = True
        is_instantiate _ = False 
        instances = filter is_instantiate stmts 

        id (Instantiate _ val _) = val

intersperse :: (a -> Compiler b) -> Compiler c -> [a] -> Compiler ()
intersperse f0 f1 [] = do
    return ()
intersperse f0 f1 (x:[]) = do
    f0 x
    return () 
intersperse f0 f1 (x:xs) = do
    f0 x
    f1
    intersperse f0 f1 xs
    return () 
    

module_io :: Statement -> Compiler ()
module_io (Instantiate t id mods) = do
    id' <- solve_id id
    case elem In mods of
        True -> add_token "input"
        _    -> return ()
    case elem Out mods of
        True -> add_token "output"
        _    -> return ()
    add_token id'

--compile statement in module
module_block_statement:: Statement -> Compiler ()
module_block_statement (Assign pos l r) = do
    add_token "assign"
    value l
    add_token "="
    value r
    add_token ";"
    return ()

--module_block_statement (Instantiate t id mod) = do
--    new_id_entry id

module_block_statement _ = do
    return ()

--compile statement in module
on_block_statement (Assign pos l r) = do
    value l
    add_token "="
    value r
    add_token ";"
    return ()


value (Bit_array_value pos l v) = 
    add_token $ (show l) ++ "'" ++ (show v)

value (Meta_number_value pos v unit) = 
    case unit of 
        ("") -> add_token (show v)
        _    -> put_error pos
            "Can not convert number literal with unit to verilogHDL."

value (Meta_string_value pos _) = 
    put_error pos
        "Can not convert string literal to verilogHDL."

value (Id_value pos id) = do
    id' <- solve_id id
    add_token id'

value (Unaly_operator pos op val) = do
    add_token "(" 
    add_token op
    value val
    add_token ")" 

value (Binaly_operator pos op l r) = do
    add_token "(" 
    value l
    add_token op
    value r
    add_token ")" 

value (Field_access pos base id) =
    put_error pos
        "Can not convert field access operator to verilogHDL."

value (Slice_operator pos val [range]) = 
    put_error pos
        "Can not convert slice op to verilogHDL."

value (Cat_operator pos vals ) = 
    put_error pos
        "Can not convert cat op to verilogHDL."
        
value (Function_call pos id vals ) = 
    put_error pos
        "Can not convert function call to verilogHDL."
