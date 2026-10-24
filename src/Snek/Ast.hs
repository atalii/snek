module Snek.Ast (Loc (..), get, tag, Program (..), Ty (..), tyListToTy, Block, Name, Literal, Args, Statement (..), Expr (..), Value (..), SnekIM, EvalState) where

import Control.Monad.State.Lazy (StateT)
import Data.List (intercalate)
import Data.Map
import Data.Text (Text, unpack)

type EvalState = Map Text Value

type SnekIM a = StateT EvalState IO a

data Value
  = IntegerV Integer
  | StrV Text
  | BoolV Bool
  | FnV Name Block
  | BuiltinV ([Expr] -> SnekIM Value)
  | NoneV
  | Tuple Value Value [Value] -- Tuples must contain at least two elements.

instance Show Value where
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

data Program = Program Block
  deriving (Show)

type Block = [Statement]

type Name = LocTagged Text

-- TODO: use a more precise type; we can't have a literal function, for instance
type Literal = LocTagged Value

type Args = [Expr]

data Ty = TyInt | TyStr | TyUnit | TyBool | Prod Ty Ty | FnArrow Ty Ty | TyAny
  deriving (Show, Eq)

tyListToTy :: [Ty] -> Ty
tyListToTy [] = TyUnit
tyListToTy [t] = t
tyListToTy (t : ts) = Prod t (tyListToTy ts)

data Statement
  = Assign Name Expr
  | Declare Name Ty Expr
  | Pass
  | Cond Expr Block Block
  | WhileLoop Expr Block
  | Return Expr
  | Invoke Expr
  deriving (Show)

data Expr
  = Lookup Name
  | Use Literal
  | Add Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  | Expn Expr Expr
  | CmpLt Expr Expr
  | CmpEq Expr Expr
  | And Expr Expr
  | Or Expr Expr
  | Not Expr
  | Fn Name Block
  | Call Expr Args
  | MkTuple Args
  deriving (Show)
