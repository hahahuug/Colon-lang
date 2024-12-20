module Colon.StackOps where

import Colon.Types

-- Положить результат на стек
push :: EvalResult -> Stack -> Stack
push result stack = result : stack

-- Вытянуть результат с вершины стека
pop :: Stack -> (Maybe EvalResult, Stack)
pop [] = (Nothing, [])  -- Если стек пуст
pop (x:xs) = (Just x, xs)  -- Вытаскиваем верхний элемент стека

-- Получить верхний элемент стека без удаления
peek :: Stack -> Maybe EvalResult
peek [] = Nothing
peek (x:_) = Just x
