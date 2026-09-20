{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Debug.Trace
import Snek.Eval (eval)
import Snek.Lex.Lexer
import Snek.Parser
import Snek.Util
import System.Environment
import System.Exit

main :: IO ()
main = (getArgs >>= fstArg >>= scanFile) >>= escalate . parse >>= eval . traceShowId
  where
    fstArg [x] = return x
    fstArg _ = putStrLn "todo: real arg parsing" >> exitWith (ExitFailure 1)
