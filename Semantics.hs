{-# LANGUAGE FlexibleInstances #-}
module Semantics where

import Token(Unit)
import Parse
import qualified Data.Map
import Data.Map (Map, insert)
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
    | Type_struct
    | Type_instance Identifier
    | Type_array    Int   S_type
    | Type_bit
    | Type_logic
    | Type_undefined
    | Type_invalid
    deriving (Show, Eq)

data S_value
    = Value_module    S_scope
    | Value_instance
    | Value_metanum  Int Unit
    --Value_bit_array n x is bit[n] = x
    | Value_bit_array Int Int
    --Array. [0, 1, .., n-1]
    | Value_array     [S_value]
    | Value_bit       S_logic
    | Value_logic     S_logic
    --Value of not defined id.
    | Value_undefined
    --Vaule of invalid expressions result like NaN.
    | Value_invalid
    deriving (Show)

data S_logic = S_0 | S_1 | S_X | S_Z
    deriving (Show)
    
data S_object = S_object S_type S_value

--Return Value_metanum with specified unit or Value_undefined if not
--compatible with that type.
cast_to_metanum :: S_value -> Unit -> S_value
cast_to_metanum (Value_metanum v "") "" = Value_metanum v ""
cast_to_metanum (Value_bit_array v l) "" = Value_metanum v ""
cast_to_metanum _ _ = Value_invalid
    
to_value :: Int -> S_value
to_value i = 
        Value_array $ map ((Value_bit).bit_to_value) (f i)
    where 
        f :: Int -> [Int]
        f 0 = [] 
        f n = (n `mod` 2 ):f (n `div` 2)
        bit_to_value 0 = S_0
        bit_to_value 1 = S_1

from_value :: S_value -> Int
from_value (Value_array vals) = 
    foldr1 (\a b -> b*2+a) (map bit_to_value vals)
    where 
        bit_to_value (Value_bit S_0) = 0
        bit_to_value (Value_bit S_1) = 1

----
--S_scope

--id-object map. Duplicative definitions will be deleted.
type Id_map    = Map Identifier Statement
type Type_map  = Map Identifier S_type
type Value_map = Map Identifier S_value

data S_scope = S_scope Id_map Type_map Value_map
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
register_type :: Identifier -> S_type -> Semantics()
register_type id t = 
   mod_scope (\ (S_scope ids ts vs) -> S_scope ids (insert id t ts) vs) 

register_value :: Identifier -> S_value -> Semantics()
register_value id v =
   mod_scope (\ (S_scope ids ts vs) -> S_scope ids ts (insert id v vs)) 

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

put_info :: String -> Semantics()
put_info = put_error dummy_pos

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
type_of (Define _ _ _)          = return $ Type_invalid
type_of (Instantiate pos t _ _)   = 
    conv t
    where 
        conv :: Type -> Semantics S_type
        conv (Array_type t length_val) = do
            t' <- conv t
            len <- solve_expr_value length_val
            case cast_to_metanum len "" of
                (Value_metanum len' "") ->
                    return $ Type_array len' t';
                _ -> do
                    put_error pos 
                       "Length of array must be compile tyme calcuratable." 
                    return Type_invalid
        conv (Unresolved_type "bit")   = return $ Type_bit
        conv (Unresolved_type "logic") = return $ Type_logic
        conv (Unresolved_type t_id)    = return $ Type_instance t_id

--Non array type can be used as 1 length array
is_array_of :: S_type -> S_type -> Bool
is_array_of base (Type_array _ t) = (base == t)
is_array_of base t = (base == t) 

length_of :: S_type -> Int
length_of (Type_array l _) = l
length_of _ = 1

--Find type in specified scope. Return Type_undefined if it was not
--defined.
solve_id_type_in :: S_scope -> Identifier -> Semantics S_type
solve_id_type_in (S_scope ids tps _) id = do
    case Data.Map.lookup id tps of
        (Just t)  -> return t
        (Nothing) -> return Type_undefined

solve_id_type_in_env :: Enviroment -> Identifier -> Semantics S_type
solve_id_type_in_env [] id = 
    return Type_undefined
solve_id_type_in_env (s:sx) id = do
    t <- solve_id_type_in s id
    case t of 
        Type_undefined -> solve_id_type_in_env sx id
        t              -> return t
    
--Find type in current enviroment.
solve_id_type :: Identifier -> Semantics S_type
solve_id_type id = do
    env  <- get_env
    solve_id_type_in_env env id

solve_id_value_in :: S_scope -> Identifier -> Semantics S_value
solve_id_value_in (S_scope ids _ vals) id = do
    case Data.Map.lookup id vals of
        (Just t)  -> return t
        (Nothing) -> return Value_undefined

solve_id_value_in_env :: Enviroment -> Identifier -> Semantics S_value
solve_id_value_in_env [] id = 
    return Value_undefined
solve_id_value_in_env (s:sx) id = do
    t <- solve_id_value_in s id
    case t of 
        Value_undefined -> solve_id_value_in_env sx id
        t               -> return t
solve_id_value :: Identifier -> Semantics S_value
solve_id_value id = do
    env  <- get_env
    solve_id_value_in_env env id

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
test_id_value  :: Expr -> Semantics()
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
    ltype <- liftM type_hack $ solve_expr_type lval
    rtype <- liftM type_hack $ solve_expr_type rval
    put_error_if (ltype /= rtype) pos 
        ("Type missmatch. Left and right value of assignment " ++
         "must be same type.")
    --put_error pos ("type(lvalue) = " ++ (show ltype))
    --put_error pos ("type(rvalue) = " ++ (show rtype))
    where
        type_hack :: S_type -> S_type
        type_hack Type_bit = Type_array 1 Type_bit
        type_hack t        = t 
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
                (S_scope ids _ _) -> ids

--Calculae type of expression.
solve_expr_type :: Expr -> Semantics S_type
solve_expr_type (Bit_array_value pos l v) = 
    return $ Type_array l Type_bit
solve_expr_type (Id_value pos id) = do
    solve_id_type id
solve_expr_type (Unaly_operator pos op val) = do
    val_type <- solve_expr_type val
    case () of
        -- ^ & |
        _ | is_to_bit op -> case () of
            _ | is_array_of Type_bit val_type-> return Type_bit
            _ -> put_error pos 
                ("Type missmatch. Operand of unaly operator " ++ op ++
                 " must be bit array") >> return Type_invalid
        -- ~ -
        _ | is_to_bit_a op -> case () of
            _ | is_array_of Type_bit val_type -> return $ val_type
            _ -> put_error pos 
                ("Type missmatch. Operand of unaly operator " ++ op ++
                 " must be bit array") >> return Type_invalid
        _ -> put_error pos ("Unknown op " ++ op ++ ".") >> 
            return Type_invalid
    where
        is_to_bit   op = elem op ["&", "|", "^"]
        is_to_bit_a op = elem op ["~", "-"]

solve_expr_type (Binaly_operator pos op lval rval) = do
    ltype <- liftM type_hack $ solve_expr_type lval
    rtype <- liftM type_hack $ solve_expr_type rval
    case () of
        _ | is_logical_op op -> logical_bi_op pos op ltype rtype
        _ | is_alithmetic_op op -> alithmetic_bi_op pos op ltype rtype
        _ -> put_error pos ("Unknown binaly operator " ++ op ++ ".") >>
            return Type_invalid
    where
        is_logical_op op = elem op ["&", "^", "|"]
        is_alithmetic_op op = elem op ["+", "-"]
        type_hack :: S_type -> S_type
        type_hack Type_bit = Type_array 1 Type_bit
        type_hack t        = t 

solve_expr_type (Field_access pos val id) = do
    val_type <- solve_expr_type val
    case val_type of
        Type_instance t_id -> do
            t_value <- solve_id_value t_id
            --t_value is type Expr of val
            case t_value of
                Value_module scope -> do
                    result <- solve_id_type_in scope id
                    put_error_if (result == Type_undefined)
                        pos ("No field named " ++ id ++ ".")
                    return result
                _ -> do
                    put_error pos "(i)Fail at type."
                    return Type_invalid
        _ -> do
            put_error pos "(i)Fail at type value."
            return Type_invalid

logical_bi_op pos op ltype rtype =
    case (ltype,rtype) of
        (Type_array ll Type_bit, Type_array lr Type_bit) | ll == lr ->
            return $ Type_array ll Type_bit
        _ ->
            put_error pos 
                ("Type missmatch. Operand of binaly operator " ++ op ++
                 " must be bit array with same length.") >>
                 put_info (show ltype) >>
                 put_info (show rtype) >>
                 return Type_invalid

alithmetic_bi_op pos op ltype rtype = do
    case (ltype,rtype) of
        (Type_array ll Type_bit, Type_array lr Type_bit) ->
            return $ Type_array (max ll lr) Type_bit
        _ ->
            put_error pos 
                ("Type missmatch. Operand of binaly operator " ++ op ++
                 " must be bit array.") >>
             return Type_invalid

{-
solve_expr_type (Binaly_operator _ _ lval rval) = do
    test_id_value lval
    test_id_value rval
solve_expr_type (Slice_operator _ val _) = 
    test_id_value val
solve_expr_type (Cat_operator _ vals) = do
    mapM test_id_value vals
    return ()
solve_expr_type (Function_call pos id vals) = do
    mapM test_id_value vals
    def <- solve_id id 
    case def of
        (Just _) -> return ()
        Nothing  -> put_error pos
            ("Missing definition of id " ++ id ++ ".")
    return ()
solve_expr_type _ = return ()
-}

solve_expr_value :: Expr -> Semantics S_value
solve_expr_value (Bit_array_value _ l v) = 
    return $ Value_bit_array l v
solve_expr_value (Meta_number_value _ v u) = 
    return $ Value_metanum v u
solve_expr_value (Id_value _ id) = do
    solve_id_value id

with_new_scope :: S_scope -> Semantics a -> Semantics S_scope
with_new_scope scope sem = do
    push_env scope
    sem
    env <- get_env
    pop_env
    return $ head env
    
statements :: [Statement] -> Semantics ()
statements stmts = do
    mapM is_id_defined   stmts
    mapM process_stmt stmts
    mapM is_type_correct stmts
    return ()
    where
        process_stmt (Define pos id (Module sub_stmts)) = do
            scope <- semantics sub_stmts
            register_value id (Value_module scope)
            return ()
        process_stmt (Instantiate pos tp id mods) = do
            out_type <- type_of (Instantiate pos tp id mods)
            register_type id out_type
        process_stmt _ = 
            return ()

semantics :: [Statement] -> Semantics S_scope
semantics stmts = do 
    -- unique id
    -- shadow id
    -- defined id
    -- type
    -- assign lvalue
    -- register clock
    is_id_unique (defines stmts)
    with_new_scope scope (statements stmts)
    where
        scope = S_scope (identifier_map stmts) Data.Map.empty Data.Map.empty

do_test = do
    s <- readFile "simple_test.lc"
    case parse_str s of
        (Right stmts) 
            -> putStr $ funcy_show $ 
                evalState (semantics stmts >> get_error) i_state
        (Left  err  ) -> print err
    where
        i_state = S_state [] 0 []
