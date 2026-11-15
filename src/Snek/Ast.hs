{-# LANGUAGE ExistentialQuantification #-}

module Snek.Ast (Loc (..), get, tag, Program (..), Ty (..), tyListToTy, Block, Name, Literal, Statement (..), Expr (..), Value (..), SnekIM, EvalState, ExprE (..), StatementE (..)) where

import Control.Monad.State.Lazy (StateT)
import Data.List (intercalate)
import Data.Map
import Data.Text (Text, unpack)

type EvalState = Map Text (Value Expr)

type SnekIM a = StateT EvalState IO a

data Value e
  = IntegerV Integer
  | StrV Text
  | BoolV Bool
  | FnV Name (Block e)
  | BuiltinV ([ExprE e] -> SnekIM (Value e))
  | NoneV
  | Tuple (Value e) (Value e) [Value e] -- Tuples must contain at least two elements.

instance Show (Value e) where
  show (IntegerV i) = show i
  show (StrV t) = unpack t
  show (BoolV b) = show b
  show (FnV a b) = show a ++ " → " ++ show b
  show NoneV = "None"
  show (BuiltinV _) = "⟨builtin⟩"
  show (Tuple a b rest) = intercalate ", " $ fmap show ([a, b] ++ rest)

data Loc = Loc {line :: Integer, column :: Integer}
  deriving (Show)

type LocTagged a = (Loc, a)

get :: LocTagged a -> a
get = snd

tag :: LocTagged a -> Loc
tag = fst

data Program = P (Block Expr)
  deriving (Show)

type Block e = [StatementE e]

type Name = LocTagged Text

-- TODO: use a more precise type; we can't have a literal function, for instance
type Literal e = LocTagged (Value e)

data Ty = TyInt | TyStr | TyUnit | TyBool | Prod Ty Ty | FnArrow Ty Ty | TyAny
  deriving (Show, Eq)

tyListToTy :: [Ty] -> Ty
tyListToTy [] = TyUnit
tyListToTy [t] = t
tyListToTy (t : ts) = Prod t (tyListToTy ts)

newtype Statement = S (StatementE Expr)
  deriving (Show)

data Expr = E (ExprE Expr)
  deriving (Show)

-- Statement and Expr types are parameterized by their child so that the same
-- tree structure can store arbitrary data.
data StatementE e
  = Assign Name (ExprE e)
  | Declare Name Ty (ExprE e)
  | Pass
  | Cond (ExprE e) (Block e) (Block e)
  | WhileLoop (ExprE e) (Block e)
  | Return (ExprE e)
  | Invoke (ExprE e)
  deriving (Show)

data ExprE e
  = Lookup Name
  | Use (Literal e)
  | Add (ExprE e) (ExprE e)
  | Mult (ExprE e) (ExprE e)
  | Div (ExprE e) (ExprE e)
  | Expn (ExprE e) (ExprE e)
  | CmpLt (ExprE e) (ExprE e)
  | CmpEq (ExprE e) (ExprE e)
  | And (ExprE e) (ExprE e)
  | Or (ExprE e) (ExprE e)
  | Not (ExprE e)
  | Fn Name (Block e)
  | Call (ExprE e) [(ExprE e)]
  | MkTuple [(ExprE e)]
  deriving (Show)
