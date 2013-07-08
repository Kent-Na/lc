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
    | Type_module
    | Type_array    Int   S_type
    | Type_struct
    | Type_bit
    | Type_logic
    | Type_undefined
    deriving (Show, Eq)

----
--S_scope

--id-object map. Duplicative definitions will be deleted.
type Id_map   = Map Identifier Statement
type Type_map = Map Identifier S_type

data S_scope = S_scope Id_map Type_map
    deriving (Show)

brank_scope = S_scope Data.Map.empty Data.Map.empty

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

get_scope :: Semantics S_scope
get_scope = liftM head get_env

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

type_of :: Statement -> Semantics S_type
type_of (Define _ _ (Module _)) = return $ Type_module
type_of (Define _ _ _)          = return $ Type_undefined 
type_of (Instantiate _ t _ _)   = 
    conv t
    where 
        conv :: Type -> Semantics S_type
        conv (Array_type t length_val) = do
            t' <- conv t
            return $ Type_array 0 t'
        conv (Unresolved_type "bit")   = return $ Type_bit
        conv (Unresolved_type "logic") = return $ Type_logic
        conv (Unresolved_type t_id)    = solve_type t_id

--Non array type can be used as 1 length array
is_array_of :: S_type -> S_type -> Bool
is_array_of base (Type_array _ t) = (base == t)
is_array_of base t = (base == t) 

solve_type :: Identifier -> Semantics S_type
solve_type id = do
    S_scope ids tps <- get_scope
    case Data.Map.lookup id tps of
        (Just t)  -> return t
        (Nothing) -> case Data.Map.lookup id ids of
            (Just stmt) -> type_of stmt
            (Nothing)   -> (put_error dummy_pos
                ("(i)Missing definition of id " ++ id ++ ".")) >>
                return Type_undefined

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

--Test id is defined or not.
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


is_type_correct :: Statement -> Semantics ()
is_type_correct (Assign pos lval rval) = do
    ltype <- solve_value_type lval
    rtype <- solve_value_type rval
    put_error_if (ltype /= rtype) pos 
        ("Type missmatch. Left and right value of assignment " ++
         "must be same type.")
is_type_correct _ = 
    return ()

solve_id :: Identifier -> Semantics (Maybe Statement)
solve_id id = do
    env <- get_env
    return $ Data.Map.lookup (id) (id_map env)
    where
        id_map :: Enviroment -> Id_map
        id_map env = 
            case head env of
                (S_scope ids _) -> ids

solve_value_type :: Value -> Semantics S_type
solve_value_type (Bit_array_value pos l v) = 
    return $ Type_array l Type_bit
solve_value_type (Id_value pos id) = do
    solve_type id
solve_value_type (Unaly_operator pos op val) = do
    val_type <- solve_value_type val
    case op of
        (_) | is_to_bit op -> case val_type of
            (_) | is_array_of Type_bit val_type-> return Type_bit
            (_) -> put_error pos 
                ("Type missmatch. Operand of unaly operator " ++ op ++
                 " must be bit array") >> return Type_undefined
        (_) | is_to_bit_a op -> case val_type of
            (_) | is_array_of Type_bit val_type -> return $ val_type
            (_) -> put_error pos 
                ("Type missmatch. Operand of unaly operator " ++ op ++
                 " must be bit array") >> return Type_undefined
    where
        is_to_bit   op = elem op ["&", "|", "^"]
        is_to_bit_a op = elem op ["~", "-"]
{-
solve_value_type (Binaly_operator _ _ lval rval) = do
    test_id_value lval
    test_id_value rval
solve_value_type (Field_access _ val _) =
    test_id_value val
solve_value_type (Slice_operator _ val _) = 
    test_id_value val
solve_value_type (Cat_operator _ vals) = do
    mapM test_id_value vals
    return ()
solve_value_type (Function_call pos id vals) = do
    mapM test_id_value vals
    def <- solve_id id 
    case def of
        (Just _) -> return ()
        Nothing  -> put_error pos
            ("Missing definition of id " ++ id ++ ".")
    return ()
solve_value_type _ = return ()
-}

with_new_scope :: S_scope -> Semantics a -> Semantics a
with_new_scope scope sem = do
    push_env scope
    out <- sem
    pop_env
    return out
    
statements :: [Statement] -> Semantics ()
statements stmts = do
    mapM is_id_defined   stmts
    mapM is_type_correct stmts
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
    -- unique id
    -- shadow id
    -- defined id
    -- type
    -- assign lvalue
    -- register clock
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
