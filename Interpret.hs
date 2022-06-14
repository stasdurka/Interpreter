module Main where

import LexTinyPlus
import ParTinyPlus
import AbsTinyPlus
import Interpreter

import ErrM

main = do
    interact calc
    putStrLn ""

calc s =
    let Ok e = pExp (myLexer s)
    in show (interpret e)