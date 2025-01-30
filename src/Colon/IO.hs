module Colon.IO where

import Colon.Interpreter
import Colon.Types
import Colon.IOCommands (dot, cr, emit, stringLiteral, key)
import System.IO (hFlush, stdout)
import Colon.Parser

-- Функция вывода состояния стека
printStack :: InterpreterState -> IO ()
printStack state = putStrLn $ "Stack: " ++ show (stack state)

-- Обработка пользовательского ввода
processInput :: InterpreterState -> String -> IO InterpreterState
processInput state input =
    case parseCommands (words input) of
        Left err -> do
            putStrLn $ "Parse error: " ++ err
            return state
        Right cmds -> do
            putStrLn $ "Stack before execution: " ++ show (stack state)  -- Выводим стек перед выполнением
            newState <- interpretIO state cmds
            putStrLn $ "Stack after execution: " ++ show (stack newState) -- Выводим стек после выполнения
            printStack newState
            -- return newState { stack = [] }  -- Очищаем стек после выполнения
            return newState

-- Основной REPL (Read-Eval-Print Loop)
repl :: InterpreterState -> IO ()
repl state = do
    putStr "> "
    hFlush stdout
    input <- getLine
    case input of
        ":quit" -> putStrLn "Goodbye!"
        ":printStack" -> printStack state >> repl state  -- Вывод стека
        _ -> do
            newState <- processInput state input
            repl newState
