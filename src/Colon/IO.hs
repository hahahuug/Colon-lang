module Colon.IO where

import Colon.Types
import Colon.Interpreter
import qualified Data.Map as Map

-- Функция для запроса значения переменной от пользователя
getVarValue :: String -> IO Int
getVarValue varName = do
    putStrLn $ "Enter value for " ++ varName ++ ":"
    input <- getLine
    return (read input :: Int)

-- Функция для запроса математического выражения от пользователя
getExpression :: IO String
getExpression = do
    putStrLn "Enter an expression to evaluate (e.g., x + 5):"
    getLine

-- Функция для вывода результата
outputResult :: EvalResult -> IO ()
outputResult result = putStrLn $ "Result: " ++ show result

-- Функция для вычисления выражения и вывода результата
evalAndOutput :: Env -> Expr -> IO ()
evalAndOutput env expr = do
    let result = eval env expr
    outputResult result
