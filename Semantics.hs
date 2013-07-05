{-# LANGUAGE FlexibleInstances #-}
module Semantics where

import Parse
import qualified Data.Map
import Data.Map (Map)
import Data.Maybe
import Text.Parsec.Pos
import Control.Monad.State


type Semantics = State S_state
data S_error = S_error SourcePos String
    deriving (Show)
data S_state = S_state Enviroment Nest_depth [S_error]
    deriving (Show)

data S_port_direction = S_in | S_out
data S_type
    = Type_function
    | Type_partial_applied_function
    | Type_module   S_scope
    | Type_array    Int   S_type
    | Type_struct   [S_type]
    | Type_bit
    | Type_logic
    deriving (Show)

----
--S_scope

--id-object map. Duplicative definitions will be deleted.
type Id_map   = Map Identifier Statement
type Type_map = Map Identifier S_type

data S_scope = S_scope Id_map Type_map
    deriving (Show)
type Enviroment = [S_scope]
type Nest_depth = Int

--state env
get_env :: Semantics Enviroment
get_env = gets (\ (S_state env _ _) -> env)

mod_env :: (Enviroment -> Enviroment) -> Semantics ()
mod_env f = modify (\ (S_state env d err) -> S_state (f env) d err)

put_env :: Enviroment -> Semantics ()
put_env env = mod_env (const env)

pop_env :: Semantics ()
pop_env = mod_env tail

push_env :: S_scope -> Semantics ()
push_env scope = mod_env (\old -> scope:old)

mod_scope :: (S_scope -> S_scope) -> Semantics ()
mod_scope f = mod_env (\env -> (f $ head env):(tail env))

--Insert the type info entry to current scope
insert_type_info :: Identifier -> S_type -> Semantics()
insert_type_info id t = 
   mod_scope (\ (S_scope ids ts) -> S_scope ids (Data.Map.insert id t ts)) 
mod_error :: ([S_error] -> [S_error]) -> Semantics ()
mod_error f = modify (\ (S_state env d err) -> S_state env d (f err))

get_error :: Semantics [S_error] 
get_error = gets (\ (S_state _ _ err) -> err)

put_error_if :: Bool -> SourcePos -> String -> Semantics ()
put_error_if cond pos message= 
    case cond of 
        True  -> mod_error (S_error pos message:)
        False -> return ()

put_error :: SourcePos -> String -> Semantics ()
put_error = put_error_if True

pos_str :: SourcePos -> String
pos_str pos = 
   (sourceName pos) ++ ":" ++ 
   ((show.sourceLine) pos) ++ ":" ++ 
   ((show.sourceColumn) pos) ++ ": "

funcy_show:: [S_error] -> String
funcy_show = 
    foldl (\a b -> a ++ (err_str b)) ""
    where
        err_str (S_error pos str)= 
            (pos_str pos) ++ str ++ "\n"

----
--core

id_of :: Statement -> Identifier
id_of (Define _ id _) = id
id_of (Instantiate _ _ id _)  =id
id_of _ = "unknown_id"

identifier_map :: [Statement] -> Id_map
identifier_map stmts = 
    Data.Map.fromList $ zip ids (defines stmts)
    where 
        ids = map id_of (defines stmts)

defines :: [Statement] -> [Statement]
defines stmts = 
    filter is_define stmts
    where
        is_define (Define _ _ _) = True
        is_define (Instantiate _ _ _ _) = True
        is_define _ = False

is_id_unique :: [Statement] -> Semantics [()]
is_id_unique stmts = 
    mapM output (Data.Map.elems i_map)
    where
        output :: [Statement] -> Semantics()
        output stmts = put_error_if (length stmts > 1) (pos_of $ head stmts)
            ("Duplicative id " ++ (id_of $ head stmts) ++ 
             " was detected." ++ concat (map to_message stmts))

        to_message :: Statement -> String
        to_message stmt = "\n\tat " ++ (pos_str $ pos_of stmt)

        i_map :: Map Identifier [Statement]
        i_map = foldl f Data.Map.empty stmts
        
        f :: Map Identifier [Statement] 
            -> Statement 
            -> Map Identifier [Statement]
        f map stmt = 
            Data.Map.insert 
                (id_of stmt) 
                (stmt : (Data.Map.findWithDefault [] (id_of stmt) map))
                map

is_id_defined :: Statement -> Semantics ()
is_id_defined (Assign _ lval rval) = do
    test_id_value lval
    test_id_value rval
is_id_defined (Return _ val) = 
    test_id_value val
is_id_defined (If_chain blocks) = do
    mapM  (\(val, _) -> test_id_value val) blocks
    return ()
is_id_defined (On cond_val pre_val post_val _) = do
    test_id_value cond_val
    test_id_value pre_val
    test_id_value post_val
is_id_defined _ = 
    return ()

test_id_value  :: Value -> Semantics()
test_id_value (Unaly_operator _ _ val) = 
    test_id_value val
test_id_value (Binaly_operator _ _ lval rval) = do
    test_id_value lval
    test_id_value rval
test_id_value (Field_access _ val _) =
    test_id_value val
test_id_value (Slice_operator _ val _) = 
    test_id_value val
test_id_value (Cat_operator _ vals) = do
    mapM test_id_value vals
    return ()
test_id_value (Function_call pos id vals) = do
    mapM test_id_value vals
    def <- solve_id id 
    case def of
        (Just _) -> return ()
        Nothing  -> put_error pos
            ("Missing definition of id " ++ id ++ ".")
    return ()
test_id_value (Id_value pos id) = do
    def <- solve_id id 
    case def of
        (Just _) -> return ()
        Nothing  -> put_error pos
            ("Missing definition of id " ++ id ++ ".")
test_id_value _ = return ()


solve_id :: Identifier -> Semantics (Maybe Statement)
solve_id id = do
    env <- get_env
    return $ Data.Map.lookup (id) (id_map env)
    where
        id_map :: Enviroment -> Id_map
        id_map env = 
            case head env of
                (S_scope ids _) -> ids

solve_value_type :: Value -> Semantics ()
test_id_value (Unaly_operator _ _ val) = 
    test_id_value val
test_id_value (Binaly_operator _ _ lval rval) = do
    test_id_value lval
    test_id_value rval
test_id_value (Field_access _ val _) =
    test_id_value val
test_id_value (Slice_operator _ val _) = 
    test_id_value val
test_id_value (Cat_operator _ vals) = do
    mapM test_id_value vals
    return ()
test_id_value (Function_call pos id vals) = do
    mapM test_id_value vals
    def <- solve_id id 
    case def of
        (Just _) -> return ()
        Nothing  -> put_error pos
            ("Missing definition of id " ++ id ++ ".")
    return ()
test_id_value (Id_value pos id) = do
    def <- solve_id id 
    case def of
        (Just _) -> return ()
        Nothing  -> put_error pos
            ("Missing definition of id " ++ id ++ ".")
test_id_value _ = return ()

with_new_scope :: S_scope -> Semantics a -> Semantics a
with_new_scope scope sem = do
    push_env scope
    out <- sem
    pop_env
    return out
    
statements :: [Statement] -> Semantics ()
statements stmts = do
    mapM is_id_defined stmts
    mapM process_stmt stmts
    return ()
    where
        process_stmt (Define pos id (Module sub_stmts)) = do
            semantics sub_stmts
            return ()
        process_stmt _ = 
            return ()

semantics :: [Statement] -> Semantics [S_error]
semantics stmts = do 
    is_id_unique (defines stmts)
    with_new_scope scope (statements stmts)
    get_error
    where
        scope = S_scope (identifier_map stmts) Data.Map.empty

do_test = do
    s <- readFile "simple_test.lc"
    case parse_str s of
        (Right stmts) 
            -> putStr $ funcy_show $ evalState (semantics stmts) i_state
        (Left  err  ) -> print err
    where
        i_state = S_state [] 0 []
