{-# LANGUAGE OverloadedStrings #-}
module Colon.Dictionary where

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M
import Data.Text (Text)

-- Определение типа словаря
type Dictionary = M.Map Text ColonValue

-- Определение возможных значений
data ColonValue = ColonInt Int | ColonString Text | ColonFunc (ColonMonad ())

instance Show ColonValue where
    show (ColonInt n) = show n
    show (ColonString s) = show s
    show (ColonFunc _) = "<function>"

-- Определение состояния
data ColonState = ColonState
    { dict :: Dictionary  -- Словарь слов
    , stack :: [ColonValue] -- Стек
    }

-- Тип монадического вычисления
type ColonMonad = StateT ColonState (ExceptT Text IO)

-- Добавление нового слова в словарь
addWord :: Text -> ColonValue -> ColonMonad ()
addWord name value = modify $ \s -> s { dict = M.insert name value (dict s) }

-- Получение значения из словаря
lookupWord :: Text -> ColonMonad ColonValue
lookupWord name = do
    d <- gets dict
    case M.lookup name d of
        Just v  -> return v
        Nothing -> throwError $ "Undefined word: " <> name

-- Пример добавления переменной
defineVariable :: Text -> ColonValue -> ColonMonad ()
defineVariable name value = addWord name value

-- Запуск интерпретатора с начальным состоянием
runColon :: ColonMonad a -> IO (Either Text a)
runColon action = runExceptT $ evalStateT action (ColonState M.empty [])

-- Тестовый запуск
main :: IO ()
main = do
    result <- runColon $ do
        addWord "double" (ColonFunc $ do
            (ColonInt x : xs) <- gets stack
            modify $ \s -> s { stack = ColonInt (2 * x) : xs })
        defineVariable "x" (ColonInt 42)
        val <- lookupWord "x"
        liftIO $ print val
    case result of
        Left err -> putStrLn $ "Error: " <> show err
        Right _  -> return ()
