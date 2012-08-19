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

module Main where

import Test.Hspec
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

-- Tests
main = hspec $
  describe "toStackCode" $ do

    it "handles integers" $ do
      toStackCode "53;" `shouldBe` Right [Push 53]
      toStackCode "63;" `shouldBe` Right [Push 63]

    it "handles addition and subtraction" $ do
      toStackCode "51+32;" `shouldBe` Right [
        Push 51,
        Push 32,
        Plus]
      toStackCode "5-31;" `shouldBe` Right [
        Push 5,
        Push 31,
        Minus]

    it "handles multiplication and division" $ do
      toStackCode "51*3;" `shouldBe` Right [
        Push 51,
        Push 3,
        Times]
      toStackCode "52/2;" `shouldBe` Right [
        Push 52,
        Push 2,
        Div]

    it "handles multiple statements" $ do
      toStackCode "11;21;" `shouldBe` Right [
        Push 11,
        Push 21]
      toStackCode "1+2;3-4;" `shouldBe` Right [
        Push 1,
        Push 2,
        Plus,
        Push 3,
        Push 4,
        Minus]

    it "handles multiple operators" $ do
      toStackCode "1+(2+3);" `shouldBe` Right [
        Push 1,
        Push 2,
        Push 3,
        Plus,
        Plus]
      toStackCode "2*(3+4);" `shouldBe` Right [
        Push 2,
        Push 3,
        Push 4,
        Plus,
        Times]

