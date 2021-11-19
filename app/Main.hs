module Main where

import SchemeParser
import ParserLib

main :: IO ()
main = do
    putStrLn "Enter the scheme program code you would like to parse:"
    code <- getLine
    putStrLn ""
    disp $ runParser program code
    where
        disp (Just ("", prog)) = do
            print prog
        disp _ = do
            putStrLn "Parse error, try again"
            putStrLn "--------"
            main