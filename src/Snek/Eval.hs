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
eval (Program stmts) = evalStateT (mapM_ run stmts) bltns >> return ()
  where
    bltns = fromList [("print", BuiltinV bPrint)]

    bPrint args = stringify args >>= liftIO . Data.Text.IO.putStrLn >> return NoneV
    stringify exprs = Data.Text.intercalate " " <$> mapM (Data.Text.show <.> getValue) exprs

set :: Name -> Value -> SnekIM ()
set name = modify . Data.Map.insert (Snek.Ast.get name)

mkFresh :: SnekIM Name
mkFresh = mkFresh' 0
  where
    mkFresh' :: Integer -> SnekIM Name
    mkFresh' x =
      gets (Data.Map.lookup $ Data.Text.show x) >>= \case
        Just _ -> mkFresh' (x + 1)
        Nothing -> return (Loc {column = 0, line = 0}, Data.Text.show x)

getv :: Name -> SnekIM (Maybe Value)
getv name = gets (Data.Map.lookup $ Snek.Ast.get name)

run :: Statement -> SnekIM Value
run (Assign name expr) = getValue expr >>= set name >> return NoneV
run Pass = return NoneV
run (Cond c t f) =
  getValue c >>= \case
    BoolV True -> Prelude.last <$> mapM run t
    BoolV False -> Prelude.last <$> mapM run f
    IntegerV i | i /= 0 -> Prelude.last <$> mapM run t
    IntegerV _ -> Prelude.last <$> mapM run f
    _ -> liftIO $ throwIO $ BadTypes
run e@(WhileLoop cond t) =
  getValue cond >>= \case
    BoolV True -> Prelude.last <$> (mapM run $ t ++ [e])
    BoolV False -> return NoneV
    _ -> liftIO $ throwIO $ BadTypes
run (Return e) = getValue e

getValue :: Expr -> StateT EvalState IO Value
getValue (Use l) = return (Snek.Ast.get l)
getValue (Lookup name) = getv name >>= unwrapOr (Unbound name)
getValue (Add l r) = do
  l' <- getValue l
  r' <- getValue r
  addVs l' r'
getValue (Mult l r) = do
  l' <- getValue l
  r' <- getValue r
  multVs l' r'
getValue (Div l r) = do
  l' <- getValue l
  r' <- getValue r
  divVs l' r'
getValue (Expn l r) = do
  l' <- getValue l
  r' <- getValue r
  expnVs l' r'
getValue (CmpLt l r) = do
  l' <- getValue l
  r' <- getValue r
  ltVs l' r'
getValue (CmpEq l r) = do
  l' <- getValue l
  r' <- getValue r
  eqVs l' r'
getValue (And l r) = do
  l' <- getValue l
  r' <- getValue r
  andVs l' r'
getValue (Or l r) = do
  l' <- getValue l
  r' <- getValue r
  orVs l' r'
getValue (Not l) = do
  l' <- getValue l
  notV l'
getValue (Fn n f) = return $ FnV n f
getValue (Call f args) =
  getValue f >>= \case
    FnV n b -> case args of
      [arg] -> do
        argValue <- getValue arg
        fresh <- mkFresh
        set fresh argValue
        Prelude.last <$> (mapM run $ Prelude.map (substS n fresh) b)
      _ -> error "todo"
    BuiltinV r -> r args
    _ -> liftIO $ throwIO $ BadTypes
-- Handle degenerate cases where the tuple syntax is used to provide the unit type or (with one element) a sub-expression.
getValue (MkTuple []) = return $ NoneV
getValue (MkTuple [x]) = getValue x
getValue (MkTuple (a : b : rest)) = liftA3 Tuple (getValue a) (getValue b) (mapM getValue rest)

substS :: Name -> Name -> Statement -> Statement
substS m n (Assign l f) | (Snek.Ast.get m) == (Snek.Ast.get l) = Assign n $ substE m n f
substS m n (Assign l f) = Assign l $ substE m n f
substS _ _ Pass = Pass
substS n e (Cond f g h) = Cond (substE n e f) (Prelude.map (substS n e) g) (Prelude.map (substS n e) h)
substS n e (WhileLoop f g) = WhileLoop (substE n e f) (Prelude.map (substS n e) g)
substS m n (Return f) = Return $ substE m n f

substE :: Name -> Name -> Expr -> Expr
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

addVs :: Value -> Value -> StateT EvalState IO Value
addVs (IntegerV lhs) (IntegerV rhs) = return $ IntegerV $ lhs + rhs
addVs (StrV lhs) (StrV rhs) = return $ StrV $ lhs <> rhs
addVs _ _ = liftIO $ throwIO $ BadTypes

multVs :: Value -> Value -> StateT EvalState IO Value
multVs (IntegerV lhs) (IntegerV rhs) = return $ IntegerV $ lhs * rhs
multVs (StrV lhs) (IntegerV rhs) = return $ StrV $ mconcat $ Prelude.take (fromIntegral rhs) $ repeat lhs
multVs lhs@(IntegerV _) rhs@(StrV _) = multVs rhs lhs
multVs _ _ = liftIO $ throwIO $ BadTypes

divVs :: Value -> Value -> StateT EvalState IO Value
divVs (IntegerV lhs) (IntegerV rhs) = return $ IntegerV $ lhs `div` rhs
divVs _ _ = liftIO $ throwIO $ BadTypes

expnVs :: Value -> Value -> StateT EvalState IO Value
expnVs (IntegerV lhs) (IntegerV rhs) = return $ IntegerV $ lhs ^ rhs
expnVs _ _ = liftIO $ throwIO $ BadTypes

ltVs :: Value -> Value -> StateT EvalState IO Value
ltVs (IntegerV lhs) (IntegerV rhs) = return $ BoolV $ lhs < rhs
ltVs _ _ = liftIO $ throwIO $ BadTypes

eqVs :: Value -> Value -> StateT EvalState IO Value
eqVs (IntegerV lhs) (IntegerV rhs) = return $ BoolV $ lhs == rhs
eqVs (StrV lhs) (StrV rhs) = return $ BoolV $ lhs == rhs
eqVs (BoolV lhs) (BoolV rhs) = return $ BoolV $ lhs == rhs
eqVs _ _ = liftIO $ throwIO $ BadTypes

andVs :: Value -> Value -> StateT EvalState IO Value
andVs (BoolV lhs) (BoolV rhs) = return $ BoolV $ lhs && rhs
andVs _ _ = liftIO $ throwIO $ BadTypes

orVs :: Value -> Value -> StateT EvalState IO Value
orVs (BoolV lhs) (BoolV rhs) = return $ BoolV $ lhs || rhs
orVs _ _ = liftIO $ throwIO $ BadTypes

notV :: Value -> StateT EvalState IO Value
notV (BoolV l) = return $ BoolV $ not l
notV _ = liftIO $ throwIO $ BadTypes
