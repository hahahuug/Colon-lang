module Main where

import Colon.IO (repl)
import Colon.Types (InterpreterState(..), emptyState)

main :: IO ()
main = do
    putStrLn "Welcome to Colon Language Interpreter!"
    repl emptyState
