module Colon.Parser where

import Colon.Types
import Data.List (stripPrefix, isSuffixOf)
import Text.Read (readMaybe)
import Debug.Trace (trace)

parseCommands :: [String] -> Either String [Command]
parseCommands [] = Right []
parseCommands ("IF":rest) =
    case break (== "ELSE") rest of
        (thenPart, "ELSE":elseRest) ->
            case break (== "THEN") elseRest of
                (elsePart, "THEN":afterThen) -> do
                    thenCmds <- parseCommands thenPart
                    elseCmds <- parseCommands elsePart
                    remainingCmds <- parseCommands afterThen
                    Right (IfElse thenCmds elseCmds : remainingCmds)
                _ -> Left "Missing THEN after ELSE"
        _ -> Left "Missing ELSE in IF ... ELSE ... THEN construct"
parseCommands ("DO":rest) =
    case break (== "LOOP") rest of
        (loopBody, "LOOP":afterLoop) -> do
            loopCmds <- parseCommands loopBody
            remainingCmds <- parseCommands afterLoop
            Right (Loop loopCmds : remainingCmds)
        _ -> Left "Missing LOOP in DO ... LOOP construct"
parseCommands (x:xs) = do
    cmd <- parseCommand x
    cmds <- parseCommands xs
    Right (cmd : cmds)

parseCommand :: String -> Either String Command
parseCommand "+" = Right Add
parseCommand "-" = Right Sub
parseCommand "*" = Right Mul
parseCommand "/" = Right Div
parseCommand "MOD" = Right Mod
parseCommand "DUP" = Right Dup
parseCommand "DROP" = Right Drop
parseCommand "SWAP" = Right Swap
parseCommand "OVER" = Right Over
parseCommand "ROT" = Right Rot
parseCommand "=" = Right Equal
parseCommand "<" = Right Less
parseCommand ">" = Right Greater
parseCommand "AND" = Right And
parseCommand "OR" = Right Or
parseCommand "INVERT" = Right Invert
parseCommand "." = Right (Word ".")
parseCommand "CR" = trace "Parsing CR command" $ Right (Word "CR")
parseCommand "EMIT" = Right Emit
parseCommand s@(x:xs)
    | x == '"' && isSuffixOf "\"" s = Right (StringLiteral (init xs))
    | otherwise = case readMaybe s of
        Just n  -> Right (Push n)
        Nothing -> Right (Execute s)