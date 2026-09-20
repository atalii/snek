module Snek.Lex.Support (Token (..), unStr) where

import Data.Text

data Token
  = Ident Text
  | StrLit Text
  | IntLit Integer
  | TokPass
  | Equals
  | QEquals
  | LT
  | Plus
  | PlusEquals
  | Star
  | DoubleSlash
  | DoubleStar
  | LParen
  | RParen
  | Newline
  | Comma
  | EOF
  | Open
  | Close
  | If
  | Else
  | While
  | Semicolon
  | LogicalAnd
  | LogicalOr
  | LogicalNot
  | Lambda
  deriving (Eq, Show)

unStr :: Token -> Text
unStr (StrLit t) = t
unStr _ = error "unStr on a non-str"
