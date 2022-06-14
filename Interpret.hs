module Main where

import LexTinyPlus
import ParTinyPlus
import AbsTinyPlus
import Interpreter
import Typechecker

import ErrM

import System.Environment
main = do
    args <- getArgs
    case args of 
      [file] -> do
            s <- readFile file
            msg <- interp s
            print $ msg
      _ -> putStrLn "Wrong number of arguments"


-- main = do
--     s <- readFile "test0.txt"
--     -- interact calc
--     msg <- calc s
--     print $ msg

interp :: String -> IO String
interp s =
    let parseResult = pProgram (myLexer s)
    in case parseResult of
        Ok p -> case typecheck p of
                Right _ -> exec p
                Left err -> return $ "compilation errror: "++ err
        _ -> return "parse error"
