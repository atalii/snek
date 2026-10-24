{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception
import Snek.Analysis
import Snek.Ast
import Snek.Eval (eval)
import Snek.Lex.Lexer
import Snek.Parser
import Snek.Util
import System.Environment
import System.Exit

main :: IO ()
main = (getArgs >>= fstArg >>= scanFile) >>= escalate . parse >>= analyze' >>= eval
  where
    analyze' p@(Program blocks) = case (analyze p) of
      Just problem -> throwIO problem
      Nothing ->
        return $
          Program $
            blocks
              ++ [ Invoke
                     ( Call
                         (Lookup (Loc {line = 0, column = 0}, "main"))
                         [Use (Loc {line = 0, column = 0}, NoneV)]
                     )
                 ]

    fstArg [x] = return x
    fstArg _ = putStrLn "todo: real arg parsing" >> exitWith (ExitFailure 1)
