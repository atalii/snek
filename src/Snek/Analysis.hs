{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Snek.Analysis (analyze, Problem (..)) where

import Control.Exception
import Control.Monad (foldM)
import Control.Monad.State.Lazy
import Data.Map
import Data.Text
import Snek.Ast
import Snek.Util

data Problem
  = UnexpectedTopLevel (StatementE Expr)
  | DefinedTwice Text
  | MismatchedReturn Text
  | ShouldReturn Text Ty
  | DoesntAlwaysReturn Text Ty
  | MismatchedDecl Text Ty Ty
  | MismatchedAssgn Name Ty Ty
  | Unbound Name
  | CondMustBeBool Ty
  | UnreachableCode
  | CantReturnEither Ty Ty
  | BadBinOp Ty Ty
  | NegationOf Ty
  | ClosuresUnimplemented
  | BadCall
  deriving (Show)

instance Exception Problem

-- We integrate return behavior into type checking, so this looks a little different as compared to Jim's.
data ReturnBehavior = NoRet | CertainRet Ty | Possibly Ty

type Defs = Map Text Ty

type Symtab = Map Text Ty

type TypecheckingM a = StateT Symtab (Either Problem) a

analyze :: Program -> Maybe Problem
analyze p = leftToMaybe $ collectDefs p >>= typecheck p

collectDefs :: Program -> Either Problem Defs
collectDefs (P stmts) = fromAList =<< mapM collectDefs' stmts
  where
    collectDefs' :: StatementE Expr -> Either Problem (Text, Ty)
    collectDefs' (Declare n t _) = Right (snd n, t)
    collectDefs' s = Left (UnexpectedTopLevel s)

    fromAList :: [(Text, Ty)] -> Either Problem Defs
    fromAList = foldM (insertOr DefinedTwice) prelude

    prelude = insert "print" (FnArrow TyAny TyUnit) Data.Map.empty

typecheck :: Program -> Defs -> Either Problem ()
typecheck (P stmts) delta = evalStateT (mapM_ (typecheckS delta . S) stmts) Data.Map.empty

typecheckB :: Defs -> Symtab -> Block Expr -> Either Problem ReturnBehavior
typecheckB delta gamma stmts = evalStateT (mapM (typecheckS delta . S) stmts) gamma >>= seqRbs

typecheckS :: Defs -> Statement -> TypecheckingM ReturnBehavior
typecheckS delta (S (Declare n (FnArrow argT retT) (Fn argN body))) = do
  gamma <- Control.Monad.State.Lazy.get
  lift (typecheckB delta (insert (snd argN) argT gamma) body) >>= \case
    CertainRet r | r == retT -> lift $ Right NoRet
    CertainRet _ -> lift $ Left $ ShouldReturn (snd n) retT
    NoRet | retT == TyUnit -> lift $ Right NoRet
    NoRet -> lift $ Left $ ShouldReturn (snd n) retT
    Possibly _ -> lift $ Left $ DoesntAlwaysReturn (snd n) retT
typecheckS delta (S (Declare n ty e)) = do
  gamma <- Control.Monad.State.Lazy.get
  ty' <- lift $ typecheckE delta gamma (E e)
  if ty == ty'
    then do
      modify (insert (snd n) ty)
      lift $ Right NoRet
    else lift $ Left (MismatchedDecl (snd n) ty ty')
typecheckS delta (S (Assign lhs rhs)) = do
  gamma <- Control.Monad.State.Lazy.get
  case gamma !? (snd lhs) of
    Nothing -> lift $ Left $ Unbound lhs
    Just ty -> do
      ty' <- lift $ typecheckE delta gamma (E rhs)
      if ty == ty' then lift (Right NoRet) else lift (Left (MismatchedAssgn lhs ty ty'))
typecheckS _ (S Pass) = lift $ Right NoRet
typecheckS delta (S (Cond c lhs rhs)) = do
  gamma <- Control.Monad.State.Lazy.get
  tyC <- lift $ typecheckE delta gamma (E c)
  if tyC /= TyBool
    then lift $ Left (CondMustBeBool tyC)
    else do
      lhsRb <- lift $ typecheckB delta gamma lhs
      rhsRb <- lift $ typecheckB delta gamma rhs
      lift $ eitherRb lhsRb rhsRb
typecheckS delta (S (WhileLoop c b)) = do
  gamma <- Control.Monad.State.Lazy.get
  tyC <- lift $ typecheckE delta gamma (E c)
  if tyC /= TyBool
    then lift $ Left (CondMustBeBool tyC)
    else do
      bRb <- lift $ typecheckB delta gamma b
      lift $ eitherRb NoRet bRb
typecheckS delta (S (Return e)) = do
  gamma <- Control.Monad.State.Lazy.get
  lift $ CertainRet <$> typecheckE delta gamma (E e)
typecheckS delta (S (Invoke e)) = do
  gamma <- Control.Monad.State.Lazy.get
  lift $ fmap (const NoRet) (typecheckE delta gamma (E e))

typecheckE :: Defs -> Symtab -> Expr -> Either Problem Ty
typecheckE delta gamma (E (Lookup name)) = maybeToEither ((gamma !? (snd name)) `orElse` (delta !? (snd name))) (Unbound name)
typecheckE _ _ (E (Use (_, IntegerV _))) = Right TyInt
typecheckE _ _ (E (Use (_, StrV _))) = Right TyStr
typecheckE _ _ (E (Use (_, BoolV _))) = Right TyBool
typecheckE _ _ (E (Use (_, _))) = error "invalid AST"
typecheckE delta gamma (E (Add lhs rhs)) = do
  lhsT <- typecheckE delta gamma (E lhs)
  rhsT <- typecheckE delta gamma (E rhs)
  case (lhsT, rhsT) of
    (TyStr, TyStr) -> Right TyStr
    (TyInt, TyInt) -> Right TyInt
    _ -> Left $ BadBinOp lhsT rhsT
typecheckE delta gamma (E (Mult lhs rhs)) = do
  lhsT <- typecheckE delta gamma (E lhs)
  rhsT <- typecheckE delta gamma (E rhs)
  case (lhsT, rhsT) of
    (TyInt, TyInt) -> Right TyInt
    (TyStr, TyInt) -> Right TyStr
    (TyInt, TyStr) -> Right TyStr
    _ -> Left $ BadBinOp lhsT rhsT
typecheckE delta gamma (E (Div lhs rhs)) = do
  lhsT <- typecheckE delta gamma (E lhs)
  rhsT <- typecheckE delta gamma (E rhs)
  case (lhsT, rhsT) of
    (TyInt, TyInt) -> Right TyInt
    _ -> Left $ BadBinOp lhsT rhsT
typecheckE delta gamma (E (Expn lhs rhs)) = do
  lhsT <- typecheckE delta gamma (E lhs)
  rhsT <- typecheckE delta gamma (E rhs)
  case (lhsT, rhsT) of
    (TyInt, TyInt) -> Right TyInt
    _ -> Left $ BadBinOp lhsT rhsT
typecheckE delta gamma (E (CmpLt lhs rhs)) = do
  lhsT <- typecheckE delta gamma (E lhs)
  rhsT <- typecheckE delta gamma (E rhs)
  case (lhsT, rhsT) of
    (TyInt, TyInt) -> Right TyBool
    (TyStr, TyStr) -> Right TyBool
    _ -> Left $ BadBinOp lhsT rhsT
typecheckE delta gamma (E (CmpEq lhs rhs)) = do
  lhsT <- typecheckE delta gamma (E lhs)
  rhsT <- typecheckE delta gamma (E rhs)
  case (lhsT, rhsT) of
    (TyInt, TyInt) -> Right TyBool
    (TyStr, TyStr) -> Right TyBool
    _ -> Left $ BadBinOp lhsT rhsT
typecheckE delta gamma (E (And lhs rhs)) = do
  lhsT <- typecheckE delta gamma (E lhs)
  rhsT <- typecheckE delta gamma (E rhs)
  case (lhsT, rhsT) of
    (TyBool, TyBool) -> Right TyBool
    _ -> Left $ BadBinOp lhsT rhsT
typecheckE delta gamma (E (Or lhs rhs)) = do
  lhsT <- typecheckE delta gamma (E lhs)
  rhsT <- typecheckE delta gamma (E rhs)
  case (lhsT, rhsT) of
    (TyBool, TyBool) -> Right TyBool
    _ -> Left $ BadBinOp lhsT rhsT
typecheckE delta gamma (E (Not x)) = do
  xT <- typecheckE delta gamma (E x)
  if xT == TyBool then Right TyBool else Left (NegationOf xT)
typecheckE _ _ (E (Fn _ _)) = Left ClosuresUnimplemented
typecheckE delta gamma (E (Call fn args)) = do
  fnT <- typecheckE delta gamma (E fn)
  argsT <- tyListToTy <$> mapM (typecheckE delta gamma . E) args
  case (fnT, argsT) of
    (FnArrow TyAny b, _) -> Right b
    (FnArrow a b, c) | a == c -> Right b
    _ -> Left BadCall
typecheckE delta gamma (E (MkTuple args)) = tyListToTy <$> mapM (typecheckE delta gamma . E) args

eitherRb :: ReturnBehavior -> ReturnBehavior -> Either Problem ReturnBehavior
eitherRb NoRet NoRet = Right NoRet
eitherRb NoRet (CertainRet t) = Right (Possibly t)
eitherRb NoRet (Possibly t) = Right (Possibly t)
eitherRb (Possibly t) NoRet = Right (Possibly t)
eitherRb (Possibly t) (Possibly v) | t == v = Right (Possibly t)
eitherRb (Possibly t) (Possibly v) = Left (CantReturnEither t v)
eitherRb (Possibly t) (CertainRet v) | t == v = Right (Possibly t)
eitherRb (Possibly t) (CertainRet v) = Left (CantReturnEither t v)
eitherRb (CertainRet t) NoRet = Right (Possibly t)
eitherRb (CertainRet t) (Possibly v) | t == v = Right (Possibly t)
eitherRb (CertainRet t) (Possibly v) = Left (CantReturnEither t v)
eitherRb (CertainRet t) (CertainRet v) | t == v = Right (CertainRet t)
eitherRb (CertainRet t) (CertainRet v) = Left (CantReturnEither t v)

seqRbs :: [ReturnBehavior] -> Either Problem ReturnBehavior
seqRbs = foldM seqRb2 NoRet

seqRb2 :: ReturnBehavior -> ReturnBehavior -> Either Problem ReturnBehavior
seqRb2 NoRet x = Right x
seqRb2 (CertainRet _) _ = Left UnreachableCode
seqRb2 (Possibly x) NoRet = Right (Possibly x)
seqRb2 (Possibly x) (CertainRet y) | x == y = Right (CertainRet y)
seqRb2 (Possibly x) (CertainRet y) = Left (CantReturnEither x y)
seqRb2 (Possibly x) (Possibly y) | x == y = Right (Possibly y)
seqRb2 (Possibly x) (Possibly y) = Left (CantReturnEither x y)
