module Colon.IOCommands where

import Colon.Types
import Debug.Trace (trace)

-- Функция интерпретатора для выполнения команд
execute :: InterpreterState -> Command -> Either String InterpreterState
execute state cmd = case cmd of
    Push n -> Right state { stack = IntVal n : stack state }
    Emit   -> emit state
    Cr     -> cr state
    Print  -> case stack state of
        (StrVal s:xs) -> Right state { stack = xs, output = output state ++ s }
        _             -> Left "Stack underflow or invalid type for Print"
    _ -> Left $ "Unknown command: " ++ show cmd

-- Вывод верхнего элемента стека и удаление его
dot :: InterpreterState -> Either String InterpreterState
dot state =
    case stack state of
        (IntVal x:xs) -> Right state {
            stack = xs,
            output = output state ++ show x ++ " " ++ "ok\n"
        }
        (StrVal s:xs) -> Right state {
            stack = xs,
            output = output state ++ s ++ " " ++ "ok\n"
        }
        (BoolVal b:xs) -> Right state {
            stack = xs,
            output = output state ++ show b ++ " " ++ "ok\n"
        }
        _ -> Left "Stack underflow"

-- Вывод перевода строки (CR)
cr :: InterpreterState -> Either String InterpreterState
cr state =
    let newOutput = output state ++ "\n"  -- добавляем новую строку
    in trace ("Executing CR\nCR ( -- )") Right state { output = newOutput }

-- Вывод символа по его ASCII-коду (EMIT)
emit :: InterpreterState -> Either String InterpreterState
emit state =
    case stack state of
        (IntVal x:xs) | x >= 0 && x <= 255 ->
            Right state { stack = xs, output = output state ++ [toEnum (fromInteger x) :: Char] }
        _ -> Left "Stack underflow or invalid character code"

-- Вывод строки (." ... ")
stringLiteral :: String -> InterpreterState -> Either String InterpreterState
stringLiteral str state = Right state { output = output state ++ str }

-- Ожидание ввода символа с клавиатуры (KEY)
key :: InterpreterState -> IO InterpreterState
key state = do
    c <- getChar
    return state { stack = IntVal (toInteger (fromEnum c)) : stack state }
