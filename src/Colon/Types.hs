module Colon.Types where

import qualified Data.Map as Map

-- Тип для выражений в языке
data Expr
  = Number Int        -- Число
  | Add Expr Expr     -- Операция сложения
  | Sub Expr Expr     -- Операция вычитания
  | Mul Expr Expr     -- Операция умножения
  | Div Expr Expr     -- Операция деления
  | Eq Expr Expr      -- Операция равенства
  | Neq Expr Expr     -- Операция неравенства
  | Gt Expr Expr      -- Операция больше
  | Lt Expr Expr      -- Операция меньше
  | Var String        -- Переменная
  | If Expr Expr Expr -- Условный оператор (если-иначе)
  | DoWhile Expr Expr -- Цикл (do-while)
  | Str String        -- Строка (новая конструкция)
  | Concat Expr Expr  -- Конкатенация строк (новая операция)
  deriving (Show, Eq)

-- Тип ошибки
data EvalError
  = DivisionByZero    -- Ошибка деления на ноль
  | UndefinedVariable String -- Ошибка неопределенной переменной
  deriving (Show, Eq)

-- Тип для результатов выполнения
data EvalResult
  = Value Int         -- Результат вычисления числа
  | StrValue String   -- Результат вычисления строки
  | Error EvalError   -- Ошибка выполнения
  deriving (Eq)

-- Реализация Show для EvalResult
instance Show EvalResult where
  show (Value v) = "Value: " ++ show v
  show (StrValue s) = "StrValue: \"" ++ s ++ "\""
  show (Error e) = "Error: " ++ show e

-- Тип для окружения переменных
type Env = Map.Map String EvalResult

-- Стек для вычислений
type Stack = [EvalResult]
