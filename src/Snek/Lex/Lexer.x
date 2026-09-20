{
{-# LANGUAGE LambdaCase #-}

module Snek.Lex.Lexer (scanFile) where

import Snek.Lex.Support
import System.IO (readFile)
import Control.Exception
import qualified Data.Text

}

%wrapper "monadUserState"

%encoding "utf-8"

$digit = [0-9]
$alpha = [a-zA-Z]

tokens :-
  "#".*                  ;
  $white+                ;
  $digit+                { \(_, _, _, d) l -> return $ IntLit $ read $ take l d }
  "pass"                 { \_ _ -> return $ TokPass }
  "if"                   { \_ _ -> (return If) :: Alex Token }
  "else"                 { \_ _ -> (return Else) :: Alex Token }
  "while"                { \_ _ -> (return While) :: Alex Token }
  $alpha($alpha|$digit)* { \(_, _, _, d) l -> return $ Ident $ Data.Text.pack $ take l d }
  \"[^\"]*\"             { \(_, _, _, d) l -> return $ StrLit $ stripQuotes $ Data.Text.pack $ take l d }
  ";"                    { \_ _ -> return Semicolon }
  "<"                    { \_ _ -> return Snek.Lex.Support.LT }
  "≟"                    { \_ _ -> return QEquals }
  "⟨"                    { \_ _ -> return Open }
  "⟩"                    { \_ _ -> return Close }
  "("                    { \_ _ -> return LParen }
  ")"                    { \_ _ -> return RParen }
  "="                    { \_ _ -> return Equals }
  ","                    { \_ _ -> return Comma }
  "+"                    { \_ _ -> return Plus }
  "+="                   { \_ _ -> return PlusEquals }
  "*"                    { \_ _ -> return Star }
  "**"                   { \_ _ -> return DoubleStar }
  "//"                   { \_ _ -> return DoubleSlash }
  "∧"                    { \_ _ -> return LogicalAnd }
  "∨"                    { \_ _ -> return LogicalOr }
  "¬"                    { \_ _ -> return LogicalNot }
  "λ"                    { \_ _ -> return Lambda }
{

data Err = Err String deriving (Show)
instance Exception Err

data AlexUserState = AlexUserState { indent_width :: Integer }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { indent_width = 0 }

stripQuotes :: Data.Text.Text -> Data.Text.Text
stripQuotes = Data.Text.tail . Data.Text.reverse . Data.Text.tail . Data.Text.reverse

alexEOF :: Alex Token
alexEOF = return EOF

lexAll :: Alex [Token]
lexAll = alexMonadScan >>= \case
  EOF -> return []
  t -> (t:) <$> lexAll

scanFile :: FilePath -> IO [Token]
scanFile path = do
  t <- readFile path
  case (runAlex t lexAll) of
    Left err -> throwIO $ Err err
    Right ts -> return ts

}
