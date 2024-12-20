module Main where

import Colon.Types
import Colon.StackOps
import Colon.Tests
import qualified Data.Map as Map

main :: IO ()
main = do
    -- Ввод значений для переменных
    putStrLn "Enter value for x:"
    x <- readLn :: IO Int
    putStrLn "Enter value for y:"
    y <- readLn :: IO Int
    putStrLn "Enter value for z:"
    z <- readLn :: IO Int
    putStrLn "Enter greeting string:"
    greeting <- getLine

    -- Создаем окружение для переменных
    let env = Map.fromList [("x", Value x), ("y", Value y), ("z", Value z), ("greeting", StrValue greeting)]
    let stack = []

    -- Запуск тестов
    testPush stack
    testAdd stack
    testSub stack
    testMul stack
    testDiv stack
    testPeek stack
    testPop stack
    testEq stack
    testNeq stack
    testRandomStackOps stack
    testConcat stack
