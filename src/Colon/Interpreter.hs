module Colon.Interpreter where

import Colon.Types
import Colon.Parser
import Data.Map (Map, lookup, insert)
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import qualified Data.Map as Map
import Debug.Trace (trace)
import Control.Monad (liftM)
import Colon.IOCommands (dot, cr, emit, stringLiteral, key)

-- Интерпретатор команд
execute :: InterpreterState -> Command -> Either String InterpreterState
execute state (Push n) = Right state { stack = IntVal n : stack state }
execute state Add = binaryOp state (+)
execute state Sub = binaryOp state (-)
execute state Mul = binaryOp state (*)
execute state Div = safeBinaryOp state div "Division by zero"
execute state Mod = safeBinaryOp state mod "Modulo by zero"
execute state (Word ".") = dot state
execute state (Word "CR") = cr state
execute state (Word "EMIT") = emit state
execute state (StringLiteral str) = stringLiteral str state
execute state (Word "KEY") = Left "KEY requires IO context, handle it in REPL"


-- Стековые операции
execute state Dup =
    case stack state of
        (x:xs) -> Right state { stack = x:x:xs }
        _ -> Left "Stack underflow"

execute state Drop =
    case stack state of
        (_:xs) -> Right state { stack = xs }
        _ -> Left "Stack underflow"

execute state Swap =
    case stack state of
        (x:y:xs) -> Right state { stack = y:x:xs }
        _ -> Left "Stack underflow"

execute state Over =
    case stack state of
        (x:y:xs) -> Right state { stack = y:x:y:xs }
        _ -> Left "Stack underflow"

execute state Rot =
    case stack state of
        (x:y:z:xs) -> Right state { stack = z:x:y:xs }  -- Переставляем элементы в нужном порядке
        _ -> Left "Stack underflow"

-- Условные операторы
execute state (IfElse thenBranch elseBranch) =
    case stack state of
        (IntVal x:xs) ->
            if x /= 0
                then interpret (state { stack = [] }) thenBranch  -- очищаем стек
                else interpret (state { stack = [] }) elseBranch
        (BoolVal b:xs) ->
            if b
                then interpret (state { stack = [] }) thenBranch  -- очищаем стек
                else interpret (state { stack = [] }) elseBranch
        _ -> Left "Stack underflow or invalid type"

-- Цикл
execute state (Loop commands) =
    case stack state of
        (IntVal end:IntVal start:xs) -> loop start end (state { stack = xs }) commands
        _ -> Left "Stack underflow or invalid type"
  where
    loop i end st cmds
        | i >= end  = Right st { stack = IntVal i : stack st }  -- Возвращаем конечное значение
        | otherwise = case interpret (st { stack = IntVal i : stack st }) cmds of
            Right newState -> loop (i + 1) end newState cmds
            Left err -> Left err


execute state (Define name cmds) =
    if Map.member name (dictionary state)
        then Left $ "Word already defined: " ++ name
        else Right state { dictionary = insert name cmds (dictionary state) }

execute state (Word w) =
    case readMaybe w of
        Just n  -> Right state { stack = IntVal n : stack state }  -- Если это число, кладем его в стек
        Nothing -> Left $ "Undefined word: " ++ w  -- Если это неизвестное слово, выдаем ошибку


execute state (StringLiteral str) =
    Right state { stack = StrVal str : stack state }

-- Операции сравнения
execute state Equal =
    case stack state of
        (IntVal x:IntVal y:xs) -> Right state { stack = BoolVal (x == y) : xs }
        _ -> Left "Stack underflow or invalid types"

execute state Greater =
    case stack state of
        (IntVal y:IntVal x:xs) -> Right state { stack = BoolVal (x > y) : xs }
        _ -> Left "Stack underflow or invalid types"

execute state Less =
    case stack state of
        (IntVal y:IntVal x:xs) -> Right state { stack = BoolVal (x < y) : xs }
        _ -> Left "Stack underflow or invalid types"

-- Логические операции
execute state And =
    case stack state of
        (BoolVal x:BoolVal y:xs) -> Right state { stack = BoolVal (x && y) : xs }
        _ -> Left "Stack underflow or invalid types"

execute state Or =
    case stack state of
        (BoolVal x:BoolVal y:xs) -> Right state { stack = BoolVal (x || y) : xs }
        _ -> Left "Stack underflow or invalid types"

execute state Invert =
    case stack state of
        (BoolVal x:xs) -> Right state { stack = BoolVal (not x) : xs }
        _ -> Left "Stack underflow or invalid type"

execute state cmd = Left $ "Unknown command: " ++ show cmd
-- Вспомогательные функции
binaryOp :: InterpreterState -> (Integer -> Integer -> Integer) -> Either String InterpreterState
binaryOp state op =
    case stack state of
        (IntVal y:IntVal x:xs) -> Right state { stack = IntVal (x `op` y) : xs }
        _ -> Left "Stack underflow or invalid types"

safeBinaryOp state op errMsg =
    case stack state of
        (IntVal y:IntVal x:xs) ->
            let result = x `op` y
            in Right state { stack = IntVal result : xs }
        _ -> Left "Stack underflow or invalid types"

-- Обработка пользовательского ввода
interpretIO :: InterpreterState -> [Command] -> IO InterpreterState
interpretIO state [] = return state
interpretIO state (Word "KEY" : cmds) = do
    newState <- key state
    interpretIO newState cmds
interpretIO state (cmd:cmds) =
    case execute state cmd of
        Left err -> do
            putStrLn err
            return state
        Right newState -> interpretIO newState cmds



interpret :: InterpreterState -> [Command] -> Either String InterpreterState
interpret state [] = Right state
interpret state (cmd:cmds) =
    trace ("Executing command: " ++ show cmd) $
    case execute state cmd of
        Left err -> Left err
        Right newState -> interpret newState cmds
