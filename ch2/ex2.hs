-- Convert a list of simple statements into
-- instructions for an abstract stack machine

-- Grammar:
--
-- program    = (statement ';')*
-- statement  = assignment | expression
-- assignment = identifier '=' expression
-- expression = term (+ term)* | term (- term)*
-- term       = factor (* factor)* | factor (/ factor)*
-- factor     = number | '(' expression ')' | identifier
-- identifier = \w+
-- number     = \d+
--

module Ch2.Ex2 where

import Control.Monad.State
import Control.Applicative
import Data.Char

toStackCode :: String -> Either String [Instruction]
toStackCode = evalStateT program
  where

    program :: Parser [Instruction]
    program = do
      look <- lookahead
      case look of
        Nothing -> return []
        _       -> (++) <$> statement <*> program

    statement = do
      expr <- expression
      match ';'
      return expr

    expression = do
      first <- term
      look <- lookahead
      case look of
        Just '+' -> do
          consume 1
          rest <- expression
          return $ first ++ rest ++ [Plus]
        Just '-' -> do
          consume 1
          rest <- expression
          return $ first ++ rest ++ [Minus]
        _   -> return first

    term = do
      first <- factor
      look <- lookahead
      case look of
        Just '*' -> do
          consume 1
          rest <- term
          return $ first ++ rest ++ [Times]
        Just '/' -> do
          consume 1
          rest <- term
          return $ first ++ rest ++ [Div]
        _   -> return first

    factor = do
      look <- lookahead
      case look of
        Just s | isDigit s -> number
        Just '(' -> do
          consume 1
          expr <- expression
          match ')'
          return expr

    number = do
      str <- takeWhile isDigit <$> get
      consume $ length str
      return [Push $ read str]

    match :: Char -> Parser ()
    match s = do
      look <- lookahead
      case look of
        Just s  -> consume 1
        _       -> error ("Failed to match character '" ++ [s] ++ "'")

    lookahead :: Parser (Maybe Char)
    lookahead = safeHead <$> get

    consume :: Int -> Parser ()
    consume n = get >>= put . drop n

    error :: String -> Parser ()
    error = lift . Left

type Parser = StateT String (Either String)

-- Primitive instructions for an abstract stack machine
data Instruction =
  Push Int |
  RValue Location |
  LValue Location |
  Pop |
  Assign |
  Copy |
  Plus |
  Minus |
  Times |
  Div |
  Mod
  deriving (Show, Eq)

type Location = String

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just $ head xs

