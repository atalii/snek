{
module Snek.Parser where

import Data.Text

import Control.Exception (Exception)
import Snek.Lex.Support
import Snek.Ast

}

%name parse
%tokentype { Token }
%error { parseError }
%monad { ParseM }

%token
  IDENT { Ident $$ }
  STR { StrLit $$ }
  INT { IntLit $$ } 
  PASS { TokPass }
  '=' { Equals }
  '≟' { QEquals }
  '<' { Snek.Lex.Support.LT }
  '+' { Plus }
  '+=' { PlusEquals }
  '*' { Star }
  '**' { DoubleStar }
  '//' { DoubleSlash }
  '(' { LParen }
  ')' { RParen }
  '⟨' { Open }
  '⟩' { Close }
  ',' { Comma }
  '∧' { LogicalAnd }
  '∨' { LogicalOr }
  '¬' { LogicalNot }
  'λ' { Lambda }
  'if' { If }
  'else' { Else }
  'while' { While }
  ';' { Semicolon }

%%

program : block { Program $1 }

block : stmt ';' { [$1] }
      | block stmt ';' { $1 ++ [$2] }

stmt : PASS { Pass }
     | IDENT '=' expr { Assign (Loc { line = 0, column = 0}, $1) $3 }
     | IDENT '+=' expr { Assign (Loc { line = 0, column = 0}, $1) (Add (Lookup (Loc { line = 0, column = 0}, $1)) $3) } 
     | 'if' expr '⟨' block '⟩' { Cond $2 $4 [Pass] }
     | 'if' expr '⟨' block '⟩' 'else' '⟨' block '⟩' { Cond $2 $4 $8 }
     | 'while' expr '⟨' block '⟩' { WhileLoop $2 $4 }
     | expr { Return $1 }

exprList : {- empty -} { [] }
	 | expr { [$1] }
	 | exprList ',' expr { $1 ++ [$3] }

expr : and { $1 }
     | expr '∨' and { Or $1 $3 }

and  : and '∧' cmp { And $1 $3 }
     | cmp { $1 }

cmp  : addn '<' addn { CmpLt $1 $3 }
     | addn '≟' addn { CmpEq $1 $3 }
     | addn { $1 }

addn : mult { $1 }
     | addn '+' mult { Add $1 $3 }

mult : expn { $1 }
     | mult '*' expn { Mult $1 $3 }
     | mult '//' expn { Div $1 $3 }

expn : neg { $1 }
     | neg '**' expn { Expn $1 $3 }

neg : fn { $1 }
    | '¬' fn { Not $2 }

fn : 'λ' IDENT '⟨' block '⟩' { Fn (Loc { line = 0, column = 0} , $2) $4 }
   | call { $1 }

call : leaf '(' exprList ')' { Call $1 $3 }
     | leaf { $1 }

leaf : STR { Use (Loc { line = 0, column = 0 }, (StrV $1)) }
     | INT { Use (Loc { line = 0, column = 0 }, (IntegerV $1)) }
     | IDENT { Lookup (Loc { line = 0, column = 0}, $1) }
     | '(' exprList ')' { MkTuple $2 }

{
data Err = Err Text
  deriving (Show)

instance Exception Err

type ParseM = Either Err

parseError :: [Token] -> ParseM a
parseError ts = Left $ Err $ pack $ "parse error: " ++ Prelude.show ts
}
