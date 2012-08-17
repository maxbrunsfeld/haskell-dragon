-- Convert a list of simple statements into
-- instructions for an abstract stack machine

-- Grammar:
--
-- program    = (statement ';')*
-- statement  = assignment | expression
-- expression = term (+ term)* | term (- term)*
-- term       = factor (* factor)* | factor (/ factor)*
-- factor     = number | '(' expression ')'
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
      return [Push expr]

    expression = do
      first <- term
      look <- lookahead
      case look of
        Just '+' -> do
          match '+'
          rest <- expression
          return $ first + rest
        Just '-' -> do
          match '-'
          rest <- expression
          return $ first - rest
        _   -> return first

    term = do
      first <- factor
      look <- lookahead
      case look of
        Just '*' -> do
          consume 1
          rest <- term
          return $ first * rest
        Just '/' -> do
          consume 1
          rest <- term
          return $ first `div` rest
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

    number :: Parser Int
    number = do
      str <- takeWhile isDigit <$> get
      consume $ length str
      return $ read str

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
  Copy
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
      toStackCode "51+32;" `shouldBe` Right [Push 83]
      toStackCode "53-31;" `shouldBe` Right [Push 22]

    it "handles multiplication and division" $ do
      toStackCode "51*3;" `shouldBe` Right [Push 153]
      toStackCode "52/2;" `shouldBe` Right [Push 26]

    it "handles multiple statements" $ do
      toStackCode "11;21;" `shouldBe` Right [Push 11, Push 21]
      toStackCode "2+2;3-3;" `shouldBe` Right [Push 4, Push 0]

    it "handles multiple operators" $ do
      toStackCode "1+(2+3);" `shouldBe` Right [Push 6]
      toStackCode "2*(3*4);" `shouldBe` Right [Push 24]

