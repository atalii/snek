module Snek.Ast (Loc (..), get, tag, Program (..), Block, Name, Literal, Args, Statement (..), Expr (..), Value (..), SnekIM, EvalState) where

import Control.Monad.State.Lazy (StateT)
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

instance Show Value where
  show (IntegerV i) = show i
  show (StrV t) = unpack t
  show (BoolV b) = show b
  show (FnV a b) = show a ++ " → " ++ show b
  show NoneV = "None"
  show (BuiltinV _) = "⟨builtin⟩"

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

type Literal = LocTagged Value

type Args = [Expr]

data Statement
  = Assign Name Expr
  | Pass
  | Cond Expr Block Block
  | WhileLoop Expr Block
  | Return Expr
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
  deriving (Show)
