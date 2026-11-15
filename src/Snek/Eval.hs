{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Snek.Eval (eval) where

import Control.Applicative (liftA3)
import Control.Exception
import Control.Monad.State.Lazy
import Data.Map
import Data.Text
import Data.Text.IO
import Snek.Ast
import Snek.Util

data EvalExc = Unbound Name | UnknownBuiltin Text | BadTypes
  deriving (Show)

instance Exception EvalExc

eval :: Program -> IO ()
eval (P stmts) = evalStateT (mapM_ (run . S) stmts) bltns >> return ()
  where
    bltns = fromList [("print", BuiltinV bPrint)]

    bPrint args = stringify args >>= liftIO . Data.Text.IO.putStrLn >> return NoneV
    stringify exprs = Data.Text.intercalate " " <$> mapM (Data.Text.show <.> getValue . E) exprs

set :: Name -> (Value Expr) -> SnekIM ()
set name = modify . Data.Map.insert (Snek.Ast.get name)

mkFresh :: SnekIM Name
mkFresh = mkFresh' 0
  where
    mkFresh' :: Integer -> SnekIM Name
    mkFresh' x =
      gets (Data.Map.lookup $ Data.Text.show x) >>= \case
        Just _ -> mkFresh' (x + 1)
        Nothing -> return (Loc {column = 0, line = 0}, Data.Text.show x)

getv :: Name -> SnekIM (Maybe (Value Expr))
getv name = gets (Data.Map.lookup $ Snek.Ast.get name)

run :: Statement -> SnekIM (Value Expr)
run (S (Assign name expr)) = getValue (E expr) >>= set name >> return NoneV
run (S Pass) = return NoneV
run (S (Cond c t f)) =
  getValue (E c) >>= \case
    BoolV True -> Prelude.last <$> mapM (run . S) t
    BoolV False -> Prelude.last <$> mapM (run . S) f
    IntegerV i | i /= 0 -> Prelude.last <$> mapM (run . S) t
    IntegerV _ -> Prelude.last <$> mapM (run . S) f
    _ -> liftIO $ throwIO $ BadTypes
run (S e@(WhileLoop cond t)) =
  getValue (E cond) >>= \case
    BoolV True -> Prelude.last <$> (mapM (run . S) $ t ++ [e])
    BoolV False -> return NoneV
    _ -> liftIO $ throwIO $ BadTypes
run (S (Return e)) = getValue (E e)
run (S (Invoke e)) = getValue (E e)
run (S (Declare n _ e)) = run (S (Assign n e))

getValue :: Expr -> StateT EvalState IO (Value Expr)
getValue (E (Use l)) = return (Snek.Ast.get l)
getValue (E (Lookup name)) = getv name >>= unwrapOr (Unbound name)
getValue (E (Add l r)) = do
  l' <- getValue (E l)
  r' <- getValue (E r)
  addVs l' r'
getValue (E (Mult l r)) = do
  l' <- getValue (E l)
  r' <- getValue (E r)
  multVs l' r'
getValue (E (Div l r)) = do
  l' <- getValue (E l)
  r' <- getValue (E r)
  divVs l' r'
getValue (E (Expn l r)) = do
  l' <- getValue (E l)
  r' <- getValue (E r)
  expnVs l' r'
getValue (E (CmpLt l r)) = do
  l' <- getValue (E l)
  r' <- getValue (E r)
  ltVs l' r'
getValue (E (CmpEq l r)) = do
  l' <- getValue (E l)
  r' <- getValue (E r)
  eqVs l' r'
getValue (E (And l r)) = do
  l' <- getValue (E l)
  r' <- getValue (E r)
  andVs l' r'
getValue (E (Or l r)) = do
  l' <- getValue (E l)
  r' <- getValue (E r)
  orVs l' r'
getValue (E (Not l)) = do
  l' <- getValue (E l)
  notV l'
getValue (E (Fn n f)) = return $ FnV n f
getValue (E (Call f args)) =
  getValue (E f) >>= \case
    FnV n b -> case args of
      [arg] -> do
        argValue <- getValue (E arg)
        fresh <- mkFresh
        set fresh argValue
        Prelude.last <$> (mapM (run . S) $ Prelude.map (substS n fresh) b)
      _ -> error "todo"
    BuiltinV r -> r args
    _ -> liftIO $ throwIO $ BadTypes
-- Handle degenerate cases where the tuple syntax is used to provide the unit type or (with one element) a sub-expression.
getValue (E (MkTuple [])) = return $ NoneV
getValue (E (MkTuple [x])) = getValue (E x)
getValue (E (MkTuple (a : b : rest))) = liftA3 Tuple (getValue (E a)) (getValue (E b)) (mapM (getValue . E) rest)

substS :: Name -> Name -> StatementE e -> StatementE e
substS m n (Assign l f) | (Snek.Ast.get m) == (Snek.Ast.get l) = Assign n $ substE m n f
substS m n (Assign l f) = Assign l $ substE m n f
substS _ _ Pass = Pass
substS n e (Cond f g h) = Cond (substE n e f) (Prelude.map (substS n e) g) (Prelude.map (substS n e) h)
substS n e (WhileLoop f g) = WhileLoop (substE n e f) (Prelude.map (substS n e) g)
substS m n (Return f) = Return $ substE m n f
substS m n (Invoke f) = Invoke $ substE m n f
substS m n (Declare s _ e) = substS m n (Assign s e)

substE :: Name -> Name -> ExprE e -> ExprE e
substE m n (Lookup l) | (Snek.Ast.get m) == (Snek.Ast.get l) = Lookup n
substE _ _ (Lookup m) = Lookup m
substE _ _ u@(Use _) = u
substE m n (Add lhs rhs) = substE m n lhs `Add` substE m n rhs
substE m n (Mult lhs rhs) = substE m n lhs `Mult` substE m n rhs
substE m n (Div lhs rhs) = substE m n lhs `Div` substE m n rhs
substE m n (Expn lhs rhs) = substE m n lhs `Expn` substE m n rhs
substE m n (CmpLt lhs rhs) = substE m n lhs `CmpLt` substE m n rhs
substE m n (CmpEq lhs rhs) = substE m n lhs `CmpEq` substE m n rhs
substE m n (And lhs rhs) = substE m n lhs `And` substE m n rhs
substE m n (Or lhs rhs) = substE m n lhs `Or` substE m n rhs
substE m n (Not lhs) = Not $ substE m n lhs
substE m n (Fn l b) | (Snek.Ast.get m) /= (Snek.Ast.get l) = Fn l (Prelude.map (substS m n) b)
substE _ _ (Fn m b) = Fn m b
substE m n (Call f gs) = Call (substE m n f) (Prelude.map (substE m n) gs)
substE m n (MkTuple vs) = MkTuple $ Prelude.map (substE m n) vs

addVs :: (Value Expr) -> (Value Expr) -> StateT EvalState IO (Value Expr)
addVs (IntegerV lhs) (IntegerV rhs) = return $ IntegerV $ lhs + rhs
addVs (StrV lhs) (StrV rhs) = return $ StrV $ lhs <> rhs
addVs _ _ = liftIO $ throwIO $ BadTypes

multVs :: (Value Expr) -> (Value Expr) -> StateT EvalState IO (Value Expr)
multVs (IntegerV lhs) (IntegerV rhs) = return $ IntegerV $ lhs * rhs
multVs (StrV lhs) (IntegerV rhs) = return $ StrV $ mconcat $ Prelude.take (fromIntegral rhs) $ repeat lhs
multVs lhs@(IntegerV _) rhs@(StrV _) = multVs rhs lhs
multVs _ _ = liftIO $ throwIO $ BadTypes

divVs :: (Value Expr) -> (Value Expr) -> StateT EvalState IO (Value Expr)
divVs (IntegerV lhs) (IntegerV rhs) = return $ IntegerV $ lhs `div` rhs
divVs _ _ = liftIO $ throwIO $ BadTypes

expnVs :: (Value Expr) -> (Value Expr) -> StateT EvalState IO (Value Expr)
expnVs (IntegerV lhs) (IntegerV rhs) = return $ IntegerV $ lhs ^ rhs
expnVs _ _ = liftIO $ throwIO $ BadTypes

ltVs :: (Value Expr) -> (Value Expr) -> StateT EvalState IO (Value Expr)
ltVs (IntegerV lhs) (IntegerV rhs) = return $ BoolV $ lhs < rhs
ltVs _ _ = liftIO $ throwIO $ BadTypes

eqVs :: (Value Expr) -> (Value Expr) -> StateT EvalState IO (Value Expr)
eqVs (IntegerV lhs) (IntegerV rhs) = return $ BoolV $ lhs == rhs
eqVs (StrV lhs) (StrV rhs) = return $ BoolV $ lhs == rhs
eqVs (BoolV lhs) (BoolV rhs) = return $ BoolV $ lhs == rhs
eqVs _ _ = liftIO $ throwIO $ BadTypes

andVs :: (Value Expr) -> (Value Expr) -> StateT EvalState IO (Value Expr)
andVs (BoolV lhs) (BoolV rhs) = return $ BoolV $ lhs && rhs
andVs _ _ = liftIO $ throwIO $ BadTypes

orVs :: (Value Expr) -> (Value Expr) -> StateT EvalState IO (Value Expr)
orVs (BoolV lhs) (BoolV rhs) = return $ BoolV $ lhs || rhs
orVs _ _ = liftIO $ throwIO $ BadTypes

notV :: (Value Expr) -> StateT EvalState IO (Value Expr)
notV (BoolV l) = return $ BoolV $ not l
notV _ = liftIO $ throwIO $ BadTypes
