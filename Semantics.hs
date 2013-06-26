{-# LANGUAGE FlexibleInstances #-}
module Semantics where

import Parse
import qualified Data.Map
import Data.Map (Map)
import Data.Maybe
import Text.Parsec.Pos

type Enviroment = [Scope]
type Nest_depth = Int
data S_error = S_error SourcePos String
    deriving (Show)
data S_state = S_state Enviroment Nest_depth [S_error]
    deriving (Show)

newtype Semantics a = Semantics{
    run_semantics :: S_state -> (a, S_state)
}

instance Monad Semantics where
    Semantics x >>= f = 
        Semantics out_func
        where out_func s_0 = (run_semantics (f a)) s_1
                where (a, s_1) = x s_0

    Semantics x >> Semantics y = 
        Semantics out_func
        where out_func s_0 = y s_1
                where (_, s_1) = x s_0

    return x = Semantics (\s -> (x, s))

semantics :: Semantics a -> Enviroment -> a
semantics sem env = 
    case (run_semantics sem) (S_state env 0 []) of
            (a , end_s) -> a
-- state
get_state :: Semantics S_state
get_state = Semantics(\s -> (s, s))

put_state :: S_state -> Semantics () 
put_state s' = Semantics(\s -> ((), s'))

--state env
get_env :: Semantics Enviroment
get_env = do
    S_state env _ _ <- get_state
    return env

put_env :: Enviroment -> Semantics () 
put_env env = do
    S_state _ d err <- get_state
    put_state $ S_state env d err

mod_env:: (Enviroment -> Enviroment) -> Semantics ()
mod_env f = do
    e  <- get_env
    put_env (f e)

push_env:: Scope -> Semantics () 
push_env s= mod_env (\env -> s:env)

pop_env:: Semantics () 
pop_env = do
    old <- get_env
    case old of
        (s:env) -> put_env env

--error
put_error:: Bool -> SourcePos -> String -> Semantics ()
put_error cond pos desc = do
    S_state a b errs <- get_state
    if cond then return () else put_state (S_state a b (err:errs))
    where err = S_error pos desc

--End of Semantics Monad
----

l_funcy_show:: [S_error] -> String
l_funcy_show = foldl (\a b -> a ++ (funcy_show b)) ""

funcy_show:: S_error -> String
funcy_show (S_error pos str)= 
    (pos_str pos) ++ str ++ "\n"
    where
        pos_str pos = 
           (sourceName pos) ++ ":" ++ 
           ((show.sourceLine) pos) ++ ":" ++ 
           ((show.sourceColumn) pos) ++ ": "

----
-- solve variable

solve_variable :: Value -> Semantics ()
solve_variable (Id_value pos n) = do
    env <- get_env
    put_error (isJust $ find_object env n)
        pos ("Undefined variable \"" ++ n ++ "\"")
    return ()
    --return $ f (result env)
    --where 
        --result env = find_variable env n
        --f (Just var) = Variable_value var
        --f Nothing    = Undefined_value

solve_variable (Unaly_operator s val) = do
    val' <- solve_variable val
    return ()
    --return $ Unaly_operator s val'

solve_variable (Binaly_operator s lval rval) = do
    lval' <- solve_variable lval
    rval' <- solve_variable rval
    return ()
    --return $ Binaly_operator s lval' rval'

solve_variable (Field_access val s) = do
    val' <- solve_variable val
    return ()
    --return $ Field_access val' s

solve_variable (Slice_operator val s) = do
    val' <- solve_variable val
    return ()
    --return $ Slice_operator val' s

solve_variable (Cat_operator vals) = do
    vals' <- mapM solve_variable vals
    return ()
    --return $ Cat_operator vals'

solve_variable (Function_call f vals) = do
    vals' <- mapM solve_variable vals
    return ()
    --return $ Function_call f vals'

solve_variable var = return ()

--solve_definition :: Enviroment -> Definition -> IO ()
--solve_definition env (Variable_value (Unresolved_variable p n)) = 
    --isJust (find_definition env n)
     -- <?> ("Undefined definition" ++ n ++ "at " ++ show p)


-- Find variable from env stack
find_object:: Enviroment -> String -> Maybe Object
find_object[] n = Nothing
find_object((Scope defs _):envx) n = 
    case Data.Map.lookup n defs of
        (Nothing) -> find_object envx n
        (Just var)-> Just var
    --where
        --f Nothing = find_object envx n
        --f (Just var) = Just var

-- Find variable from env stack
--find_variable :: Enviroment -> String -> Maybe Variable
--find_variable [] n = Nothing
--find_variable ((Scope _ vars _):envx) n = 
    --f (Data.Map.lookup n vars)
    --where
        --f Nothing = find_variable envx n
        --f (Just var) = Just var

--verify :: Statement -> IO () 
len::Value -> Int
len (Bit_array_value l _) = l

is_lvalue :: Value -> Bool
is_lvalue (Id_value _ _) = True
is_lvalue (Field_access v _) = is_lvalue v
is_lvalue (Slice_operator v _) = is_lvalue v
is_lvalue (Cat_operator vs) = and (map is_lvalue vs)
is_lvalue _ = False

class Parser_elem a where
    solve:: a -> Semantics a
    verify :: a -> Semantics ()

instance Parser_elem Scope where
    solve (Scope defs stmts) = do
        push_env $ Scope defs stmts
        defs' <- mapM solve (Data.Map.elems defs) 
        --mapM (verify env) (Data.Map.elems vars)
        stmts' <- mapM solve stmts
        pop_env
        return $ Scope defs stmts

    verify (Scope defs stmts) = do
        push_env $ Scope defs stmts
        mapM verify (Data.Map.elems defs) 
        --mapM (verify env) (Data.Map.elems vars)
        mapM verify stmts
        pop_env
        return ()
    
instance Parser_elem Object where
    solve (Module s) = do
        s' <- solve s
        return $ Module s'
    solve var = do
        return $ var

    verify (Module s) = do
        verify s

    verify _ = do return ()

instance Parser_elem Statement where
    solve (Assign pos l r) = do
        l' <- solve_variable l
        r' <- solve_variable r
        --return $ Assign pos l' r'
        return $ Assign pos l r

    solve a = return a

    verify (Assign pos l r) = do
        put_error (is_lvalue l) pos 
            "Can't assign to non variable value."
        --((len l) == len r) <?> 
            --"Left and right value of assignment must be same length."
    verify _ = return ()

--instance Parser_elem (Either a Scope) where
    --verify (Right s) = do
        --verify s
        --put_error (True) (newPos "" 0 0)
            --"No varid parser output."
    --verify _ _ = print "Fail in parse stage."
   
verify_root :: Semantics [S_error]
verify_root = do
    S_state [env] a b <- get_state
    env' <- solve env
    put_env [env']
    verify env'
    S_state _ _ e <- get_state
    st <- get_state

    return e

verify_str_file n = do
    s <- readFile "RS232.lc"
    case parse_str s of
        Right env -> (putStr.l_funcy_show) (semantics verify_root [env])
        Left err -> print err
    print "done"

verify_str = 
    verify_str_file "RS232.lc"
