module Colon.Types where

import Data.Map (Map, empty)

type Stack = [Value]
type Dictionary = Map String [Command]

data Value
    = IntVal Integer
    | StrVal String
    | BoolVal Bool
    deriving (Show, Eq)

data Command
    = Push Integer
    | Add | Sub | Mul | Div | Mod
    | Dup | Drop | Swap | Over | Rot
    | Equal | Less | Greater | And | Or | Invert
    | IfElse [Command] [Command]
    | Loop [Command]
    | Define String [Command]
    | Execute String
    | Print | Emit | Cr
    | Key
    | StringLiteral String
    | DotQuote String
    | Word String  -- Конструктор для имени слова
    | Semicolon    -- Завершение определения слова
    deriving (Show, Eq)

data InterpreterState = InterpreterState
    { stack :: Stack
    , dictionary :: Dictionary
    , output :: String
    } deriving (Show, Eq)

emptyState :: InterpreterState
emptyState = InterpreterState { stack = [], dictionary = empty, output = "" }

