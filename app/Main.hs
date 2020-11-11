module Main where

import System.Environment (getArgs)

import Text.Pretty.Simple (pPrint)

import Lib

main :: IO ()
main = do
    args <- getArgs
    source <- getContents
    let result = runParser "" source
    let verbose = ("-v" `elem` args) || ("--verbose" `elem` args)
    if verbose
        then pPrint result
        else putStrLn $ shw result
    where shw (Left err) = show err
          shw (Right ok) = pprint ok
