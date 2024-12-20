module Colon.Tests where

import Colon.Types
import Colon.Interpreter
import Colon.StackOps
import qualified Data.Map as Map

-- Тест для добавления числа в стек
testPush :: Stack -> IO ()
testPush stack = do
    let result = Value 10
    let stackAfterPush = push result stack
    putStrLn $ "Test Push: Pushed " ++ show result
    putStrLn $ "Stack after pushing: " ++ show stackAfterPush

-- Тест для сложения двух чисел на стеке
testAdd :: Stack -> IO ()
testAdd stack = do
    let stack1 = push (Value 10) stack
    let stack2 = push (Value 20) stack1
    let (top1, stackAfterPop1) = pop stack2
    let (top2, stackAfterPop2) = pop stackAfterPop1
    case (top1, top2) of
        (Just (Value v1), Just (Value v2)) -> putStrLn $ "Test Add: Sum = " ++ show (v1 + v2)
        _ -> putStrLn "Test Add: Error - Stack doesn't contain valid numbers"

-- Тест для вычитания двух чисел на стеке
testSub :: Stack -> IO ()
testSub stack = do
    let stack1 = push (Value 30) stack
    let stack2 = push (Value 20) stack1
    let (top1, stackAfterPop1) = pop stack2
    let (top2, stackAfterPop2) = pop stackAfterPop1
    case (top1, top2) of
        (Just (Value v1), Just (Value v2)) -> putStrLn $ "Test Sub: Difference = " ++ show (v1 - v2)
        _ -> putStrLn "Test Sub: Error - Stack doesn't contain valid numbers"

-- Тест для умножения двух чисел на стеке
testMul :: Stack -> IO ()
testMul stack = do
    let stack1 = push (Value 5) stack
    let stack2 = push (Value 4) stack1
    let (top1, stackAfterPop1) = pop stack2
    let (top2, stackAfterPop2) = pop stackAfterPop1
    case (top1, top2) of
        (Just (Value v1), Just (Value v2)) -> putStrLn $ "Test Mul: Product = " ++ show (v1 * v2)
        _ -> putStrLn "Test Mul: Error - Stack doesn't contain valid numbers"

-- Тест для деления двух чисел на стеке
testDiv :: Stack -> IO ()
testDiv stack = do
    let stack1 = push (Value 20) stack
    let stack2 = push (Value 4) stack1
    let (top1, stackAfterPop1) = pop stack2
    let (top2, stackAfterPop2) = pop stackAfterPop1
    case (top1, top2) of
        (Just (Value v1), Just (Value v2)) ->
            if v2 == 0
                then putStrLn "Test Div: Error - Division by zero"
                else putStrLn $ "Test Div: Quotient = " ++ show (v1 `div` v2)
        _ -> putStrLn "Test Div: Error - Stack doesn't contain valid numbers"

-- Тест для печати верхнего элемента стека
testPeek :: Stack -> IO ()
testPeek stack = do
    let stack1 = push (Value 10) stack
    let top = peek stack1
    putStrLn $ "Test Peek: Top of stack = " ++ show top

-- Тест для удаления верхнего элемента стека
testPop :: Stack -> IO ()
testPop stack = do
    let stack1 = push (Value 10) stack
    let (top, stackAfterPop) = pop stack1
    putStrLn $ "Test Pop: Popped value = " ++ show top
    putStrLn $ "Stack after pop: " ++ show stackAfterPop

-- Тест для проверки равенства двух чисел на стеке
testEq :: Stack -> IO ()
testEq stack = do
    let stack1 = push (Value 10) stack
    let stack2 = push (Value 10) stack1
    let (top1, stackAfterPop1) = pop stack2
    let (top2, stackAfterPop2) = pop stackAfterPop1
    case (top1, top2) of
        (Just (Value v1), Just (Value v2)) -> putStrLn $ "Test Eq: Equal = " ++ show (if v1 == v2 then 1 else 0)
        _ -> putStrLn "Test Eq: Error - Stack doesn't contain valid numbers"

-- Тест для проверки неравенства двух чисел на стеке
testNeq :: Stack -> IO ()
testNeq stack = do
    let stack1 = push (Value 10) stack
    let stack2 = push (Value 20) stack1
    let (top1, stackAfterPop1) = pop stack2
    let (top2, stackAfterPop2) = pop stackAfterPop1
    case (top1, top2) of
        (Just (Value v1), Just (Value v2)) -> putStrLn $ "Test Neq: Not Equal = " ++ show (if v1 /= v2 then 1 else 0)
        _ -> putStrLn "Test Neq: Error - Stack doesn't contain valid numbers"

-- Тест для рандомизированных операций с числовыми значениями
testRandomStackOps :: Stack -> IO ()
testRandomStackOps stack = do
    let stack1 = push (Value 5) stack
    let stack2 = push (Value 10) stack1
    let (top, stackAfterPop) = pop stack2
    putStrLn $ "Random test, popped value: " ++ show top
    putStrLn $ "Stack after pop: " ++ show stackAfterPop

-- Тест для конкатенации строк
testConcat :: Stack -> IO ()
testConcat stack = do
    let stack1 = push (StrValue "Hello") stack
    let stack2 = push (StrValue " World") stack1
    let (top1, stackAfterPop1) = pop stack2
    let (top2, stackAfterPop2) = pop stackAfterPop1
    case (top1, top2) of
        (Just (StrValue s1), Just (StrValue s2)) -> putStrLn $ "Test Concat: Concatenated = " ++ (s1 ++ s2)
        _ -> putStrLn "Test Concat: Error - Stack doesn't contain valid strings"
