module Colon.Interpreter where

import Colon.Types
import qualified Data.Map as Map

-- Функция для выполнения арифметических операций и сравнений
eval :: Env -> Expr -> EvalResult
eval env (Number n) = Value n
eval env (Add e1 e2) = evalExpr env e1 `add` evalExpr env e2
eval env (Sub e1 e2) = evalExpr env e1 `sub` evalExpr env e2
eval env (Mul e1 e2) = evalExpr env e1 `mul` evalExpr env e2
eval env (Div e1 e2) = case (evalExpr env e1, evalExpr env e2) of
    (Value v1, Value 0) -> Error DivisionByZero
    (Value v1, Value v2) -> evalDiv v1 v2
    (Error e, _) -> Error e
    (_, Error e) -> Error e
eval env (Eq e1 e2) = case (evalExpr env e1, evalExpr env e2) of
    (Value v1, Value v2) -> Value (if v1 == v2 then 1 else 0)
    (StrValue s1, StrValue s2) -> Value (if s1 == s2 then 1 else 0)
    (Error e, _) -> Error e
    (_, Error e) -> Error e
eval env (Neq e1 e2) = case (evalExpr env e1, evalExpr env e2) of
    (Value v1, Value v2) -> Value (if v1 /= v2 then 1 else 0)
    (StrValue s1, StrValue s2) -> Value (if s1 /= s2 then 1 else 0)
    (Error e, _) -> Error e
    (_, Error e) -> Error e
eval env (Gt e1 e2) = case (evalExpr env e1, evalExpr env e2) of
    (Value v1, Value v2) -> Value (if v1 > v2 then 1 else 0)
    (Error e, _) -> Error e
    (_, Error e) -> Error e
eval env (Lt e1 e2) = case (evalExpr env e1, evalExpr env e2) of
    (Value v1, Value v2) -> Value (if v1 < v2 then 1 else 0)
    (Error e, _) -> Error e
    (_, Error e) -> Error e
eval env (Var name) = case Map.lookup name env of
    Just val -> val
    Nothing -> Error (UndefinedVariable name)
eval env (Str s) = StrValue s
eval env (Concat e1 e2) = case (evalExpr env e1, evalExpr env e2) of
    (StrValue s1, StrValue s2) -> StrValue (s1 ++ s2)
    (Error e, _) -> Error e
    (_, Error e) -> Error e

-- Вспомогательная функция для вычисления выражений
evalExpr :: Env -> Expr -> EvalResult
evalExpr env (Number n) = Value n
evalExpr env (Add e1 e2) = evalExpr env e1 `add` evalExpr env e2
evalExpr env (Sub e1 e2) = evalExpr env e1 `sub` evalExpr env e2
evalExpr env (Mul e1 e2) = evalExpr env e1 `mul` evalExpr env e2
evalExpr env (Div e1 e2) = case (evalExpr env e1, evalExpr env e2) of
    (Value v1, Value v2) -> evalDiv v1 v2
    (Error e, _) -> Error e
    (_, Error e) -> Error e
evalExpr env (Eq e1 e2) = case (evalExpr env e1, evalExpr env e2) of
    (Value v1, Value v2) -> Value (if v1 == v2 then 1 else 0)
    (StrValue s1, StrValue s2) -> Value (if s1 == s2 then 1 else 0)
    (Error e, _) -> Error e
    (_, Error e) -> Error e
evalExpr env (Neq e1 e2) = case (evalExpr env e1, evalExpr env e2) of
    (Value v1, Value v2) -> Value (if v1 /= v2 then 1 else 0)
    (StrValue s1, StrValue s2) -> Value (if s1 /= s2 then 1 else 0)
    (Error e, _) -> Error e
    (_, Error e) -> Error e
evalExpr env (Gt e1 e2) = case (evalExpr env e1, evalExpr env e2) of
    (Value v1, Value v2) -> Value (if v1 > v2 then 1 else 0)
    (Error e, _) -> Error e
    (_, Error e) -> Error e
evalExpr env (Lt e1 e2) = case (evalExpr env e1, evalExpr env e2) of
    (Value v1, Value v2) -> Value (if v1 < v2 then 1 else 0)
    (Error e, _) -> Error e
    (_, Error e) -> Error e
evalExpr env (Var name) = case Map.lookup name env of
    Just val -> val
    Nothing -> Error (UndefinedVariable name)
evalExpr env (Str s) = StrValue s
evalExpr env (Concat e1 e2) = case (evalExpr env e1, evalExpr env e2) of
    (StrValue s1, StrValue s2) -> StrValue (s1 ++ s2)
    (Error e, _) -> Error e
    (_, Error e) -> Error e

-- Операции
add :: EvalResult -> EvalResult -> EvalResult
add (Value v1) (Value v2) = Value (v1 + v2)
add _ _ = Error DivisionByZero  -- Ошибка

sub :: EvalResult -> EvalResult -> EvalResult
sub (Value v1) (Value v2) = Value (v1 - v2)
sub _ _ = Error DivisionByZero  -- Ошибка

mul :: EvalResult -> EvalResult -> EvalResult
mul (Value v1) (Value v2) = Value (v1 * v2)
mul _ _ = Error DivisionByZero  -- Ошибка

evalDiv :: Int -> Int -> EvalResult
evalDiv v1 v2
  | v2 == 0 = Error DivisionByZero
  | otherwise = Value (v1 `div` v2)
