module Tests where

import Colon.Interpreter
import Colon.Types
import Colon.Parser
import Test.HUnit
import qualified Data.Map as Map

-- Тестирование базовых арифметических операций
arithmeticTests :: [Test]
arithmeticTests =
    [ testAdd
    , testSub
    , testMul
    , testDiv
    , testMod
    , testInvalidDiv
    , testInvalidMod
    ]

stackTests :: [Test]
stackTests =
    [ testPush
    , testDup
    , testDrop
    , testSwap
    , testOver
    , testRot
    ]

-- Тестирование сравнений
comparisonTests :: [Test]
comparisonTests =
    [ testEq
    , testGt
    , testLt
    , testAnd
    , testOr
    , testInvert
    ]

-- Тестирование условных операторов
conditionalTests :: [Test]
conditionalTests =
    [ testIfTrue
    , testIfFalse
    ]

-- Тестирование циклов
loopTests :: [Test]
loopTests =
    [ testDoLoop
    ]

-- Сложение
testAdd :: Test
testAdd = TestCase $ do
    let state = InterpreterState { stack = [IntVal 1, IntVal 2], dictionary = Map.empty, output = "" }
    let result = execute state Add
    assertEqual "Addition should return correct result"
        (Right (InterpreterState { stack = [IntVal 3], dictionary = Map.empty, output = "" })) result

-- Вычитание
testSub :: Test
testSub = TestCase $ do
    let state = InterpreterState { stack = [IntVal 3, IntVal 9], dictionary = Map.empty, output = "" }
    let result = execute state Sub
    assertEqual "Subtraction should return correct result"
        (Right (InterpreterState { stack = [IntVal 6], dictionary = Map.empty, output = "" })) result

-- Умножение
testMul :: Test
testMul = TestCase $ do
    let state = InterpreterState { stack = [IntVal 3, IntVal 4], dictionary = Map.empty, output = "" }
    let result = execute state Mul
    assertEqual "Multiplication should return correct result"
        (Right (InterpreterState { stack = [IntVal 12], dictionary = Map.empty, output = "" })) result

-- Деление
testDiv :: Test
testDiv = TestCase $ do
    let state = InterpreterState { stack = [IntVal 4, IntVal 12], dictionary = Map.empty, output = "" }
    let result = execute state Div
    assertEqual "Division should return correct result"
        (Right (InterpreterState { stack = [IntVal 3], dictionary = Map.empty, output = "" })) result

-- Модуль
testMod :: Test
testMod = TestCase $ do
    let state = InterpreterState { stack = [IntVal 5, IntVal 12], dictionary = Map.empty, output = "" }
    let result = execute state Mod
    assertEqual "Modulo should return correct result"
        (Right (InterpreterState { stack = [IntVal 2], dictionary = Map.empty, output = "" })) result

-- Ошибка деления на ноль
testInvalidDiv :: Test
testInvalidDiv = TestCase $ do
    let state = InterpreterState { stack = [IntVal 0, IntVal 12], dictionary = Map.empty, output = "" }
    let result = execute state Div
    assertEqual "Division by zero should return error"
        (Left "Division by zero") result

-- Ошибка модуля на ноль
testInvalidMod :: Test
testInvalidMod = TestCase $ do
    let state = InterpreterState { stack = [IntVal 0, IntVal 12], dictionary = Map.empty, output = "" }
    let result = execute state Mod
    assertEqual "Modulo by zero should return error"
        (Left "Modulo by zero") result

-- Тесты операций со стеком
testPush :: Test
testPush = TestCase $ do
    let initialState = InterpreterState { stack = [], dictionary = Map.empty, output = "" }
    let result = execute initialState (Push 10)
    case result of
        Right newState -> assertEqual "Stack after push" [IntVal 10] (stack newState)
        Left err -> assertFailure err

testDup :: Test
testDup = TestCase $ do
    let initialState = InterpreterState { stack = [IntVal 10], dictionary = Map.empty, output = "" }
    let result = execute initialState Dup
    case result of
        Right newState -> assertEqual "Stack after dup" [IntVal 10, IntVal 10] (stack newState)
        Left err -> assertFailure err

testDrop :: Test
testDrop = TestCase $ do
    let initialState = InterpreterState { stack = [IntVal 10], dictionary = Map.empty, output = "" }
    let result = execute initialState Drop
    case result of
        Right newState -> assertEqual "Stack after drop" [] (stack newState)
        Left err -> assertFailure err

testSwap :: Test
testSwap = TestCase $ do
    let initialState = InterpreterState { stack = [IntVal 10, IntVal 20], dictionary = Map.empty, output = "" }
    let result = execute initialState Swap
    case result of
        Right newState -> assertEqual "Stack after swap" [IntVal 20, IntVal 10] (stack newState)
        Left err -> assertFailure err

testOver :: Test
testOver = TestCase $ do
    let initialState = InterpreterState { stack = [IntVal 10, IntVal 20], dictionary = Map.empty, output = "" }
    let result = execute initialState Over
    case result of
        Right newState -> assertEqual "Stack after over" [IntVal 20, IntVal 10, IntVal 20] (stack newState)
        Left err -> assertFailure err

testRot :: Test
testRot = TestCase $ do
    let initialState = InterpreterState { stack = [IntVal 10, IntVal 20, IntVal 30], dictionary = Map.empty, output = "" }
    let result = execute initialState Rot
    case result of
        Right newState -> assertEqual "Stack after rot" [IntVal 30, IntVal 10, IntVal 20] (stack newState)
        Left err -> assertFailure err


-- Тесты для сравнений
testEq :: Test
testEq = TestCase $ do
    let state = InterpreterState { stack = [IntVal 5, IntVal 5], dictionary = Map.empty, output = "" }
    let result = execute state Equal
    assertEqual "Equality should return true"
        (Right (InterpreterState { stack = [BoolVal True], dictionary = Map.empty, output = "" })) result

testGt :: Test
testGt = TestCase $ do
    let state = InterpreterState { stack = [IntVal 5, IntVal 10], dictionary = Map.empty, output = "" }
    let result = execute state Greater
    assertEqual "Greater than should return true"
        (Right (InterpreterState { stack = [BoolVal True], dictionary = Map.empty, output = "" })) result

testLt :: Test
testLt = TestCase $ do
    let state = InterpreterState { stack = [IntVal 10, IntVal 5], dictionary = Map.empty, output = "" }
    let result = execute state Less
    assertEqual "Less than should return true"
        (Right (InterpreterState { stack = [BoolVal True], dictionary = Map.empty, output = "" })) result

testAnd :: Test
testAnd = TestCase $ do
    let state = InterpreterState { stack = [BoolVal True, BoolVal False], dictionary = Map.empty, output = "" }
    let result = execute state And
    assertEqual "AND should return false"
        (Right (InterpreterState { stack = [BoolVal False], dictionary = Map.empty, output = "" })) result

testOr :: Test
testOr = TestCase $ do
    let state = InterpreterState { stack = [BoolVal True, BoolVal False], dictionary = Map.empty, output = "" }
    let result = execute state Or
    assertEqual "OR should return true"
        (Right (InterpreterState { stack = [BoolVal True], dictionary = Map.empty, output = "" })) result

testInvert :: Test
testInvert = TestCase $ do
    let state = InterpreterState { stack = [BoolVal False], dictionary = Map.empty, output = "" }
    let result = execute state Invert
    assertEqual "Invert should return true"
        (Right (InterpreterState { stack = [BoolVal True], dictionary = Map.empty, output = "" })) result

-- Тесты для условных операторов
testIfTrue :: Test
testIfTrue = TestCase $ do
    let state = InterpreterState { stack = [BoolVal True, IntVal 10], dictionary = Map.empty, output = "" }
    let result = execute state (IfElse [Push 1] [Push 2])
    assertEqual "If should execute first branch"
        (Right (InterpreterState { stack = [IntVal 1], dictionary = Map.empty, output = "" })) result

testIfFalse :: Test
testIfFalse = TestCase $ do
    let state = InterpreterState { stack = [BoolVal False, IntVal 10], dictionary = Map.empty, output = "" }
    let result = execute state (IfElse [Push 1] [Push 2])
    assertEqual "If should execute second branch"
        (Right (InterpreterState { stack = [IntVal 2], dictionary = Map.empty, output = "" })) result

-- Тестирование цикла
testDoLoop :: Test
testDoLoop = TestCase $ do
    let state = InterpreterState { stack = [IntVal 0, IntVal 3], dictionary = Map.empty, output = "" }
    let result = execute state (Loop [Push 1])
    assertEqual "Loop should execute 3 times"
        (Right (InterpreterState { stack = [IntVal 3], dictionary = Map.empty, output = "" })) result

-- Функция main для запуска тестов
main :: IO ()
main = do
    runTestTT (TestList (arithmeticTests ++ stackTests ++ comparisonTests ++ conditionalTests ++ loopTests)) >> return ()