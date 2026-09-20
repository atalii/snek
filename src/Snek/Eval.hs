{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Snek.Eval (eval) where

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

run :: Statement -> SnekIM Value
run (Assign name expr) = getValue expr >>= modify . Data.Map.insert (Snek.Ast.get name) >> return NoneV
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
getValue (Lookup name) = gets (Data.Map.lookup (Snek.Ast.get name)) >>= unwrapOr (Unbound name)
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
      [arg] -> Prelude.last <$> (mapM run $ Prelude.map (substS n arg) b)
      _ -> error "todo"
    BuiltinV r -> r args
    _ -> liftIO $ throwIO $ BadTypes

substS :: Name -> Expr -> Statement -> Statement
substS n e (Assign m f) = Assign m $ substE n e f
substS _ _ Pass = Pass
substS n e (Cond f g h) = Cond (substE n e f) (Prelude.map (substS n e) g) (Prelude.map (substS n e) h)
substS n e (WhileLoop f g) = WhileLoop (substE n e f) (Prelude.map (substS n e) g)
substS n e (Return f) = Return $ substE n e f

substE :: Name -> Expr -> Expr -> Expr
substE n e (Lookup n') | (Snek.Ast.get n) == (Snek.Ast.get n') = e
substE _ _ (Lookup n') = Lookup n'
substE _ _ u@(Use _) = u
substE n e (Add lhs rhs) = substE n e lhs `Add` substE n e rhs
substE n e (Mult lhs rhs) = substE n e lhs `Mult` substE n e rhs
substE n e (Div lhs rhs) = substE n e lhs `Div` substE n e rhs
substE n e (Expn lhs rhs) = substE n e lhs `Expn` substE n e rhs
substE n e (CmpLt lhs rhs) = substE n e lhs `CmpLt` substE n e rhs
substE n e (CmpEq lhs rhs) = substE n e lhs `CmpEq` substE n e rhs
substE n e (And lhs rhs) = substE n e lhs `And` substE n e rhs
substE n e (Or lhs rhs) = substE n e lhs `Or` substE n e rhs
substE n e (Not lhs) = Not $ substE n e lhs
substE n e (Fn n' b) | (Snek.Ast.get n) /= (Snek.Ast.get n') = Fn n' (Prelude.map (substS n e) b)
substE _ _ (Fn n' b) = Fn n' b
substE n e (Call f gs) = Call (substE n e f) (Prelude.map (substE n e) gs)

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
