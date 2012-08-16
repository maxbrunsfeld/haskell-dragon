-- Convert arithmetic expressions to instructions
-- for an abstract stack machine

-- Grammar:
--
-- program    = statement*
-- statement  = expression ';'
-- expression = term (+ term)* | term (- term)*
-- term       = factor (* factor)* | factor (/ factor)*
-- factor     = number | '(' expression ')'
--

module Main where

import Test.Hspec
import Control.Monad.State
import Control.Applicative
import Data.Char

expressionToStackCode :: String -> Either String [Instruction]
expressionToStackCode = evalStateT program
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
      term1 <- term
      lookahead_char <- lookahead
      case lookahead_char of
        Just '+' -> do
          match '+'
          term2 <- term
          return [Push $ term1 + term2]
        Just '-' -> do
          match '-'
          term2 <- term
          return [Push $ term1 - term2]
        _   -> return [Push term1]

    term = do
      fact1 <- factor
      lookahead_char <- lookahead
      case lookahead_char of
        Just '*' -> do
          consume 1
          fact2 <- factor
          return $ fact1 * fact2
        Just '/' -> do
          consume 1
          fact2 <- factor
          return $ fact1 `div` fact2
        _   -> return fact1

    factor = number

    number :: Parser Int
    number = do
      str <- takeWhile isDigit <$> get
      consume $ length str
      return $ read str

    match :: Char -> Parser ()
    match s = do
      lookahead_char <- lookahead
      case lookahead_char of
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
  describe "expressionToStackCode" $ do

    it "handles integers" $ do
      expressionToStackCode "53;" `shouldBe` Right [Push 53]
      expressionToStackCode "63;" `shouldBe` Right [Push 63]

    it "handles addition and subtraction" $ do
      expressionToStackCode "51+32;" `shouldBe` Right [Push 83]
      expressionToStackCode "53-31;" `shouldBe` Right [Push 22]

    it "handles multiplication and division" $ do
      expressionToStackCode "51*3;" `shouldBe` Right [Push 153]
      expressionToStackCode "52/2;" `shouldBe` Right [Push 26]

    it "handles multiple statements" $ do
      expressionToStackCode "11;21;" `shouldBe` Right [Push 11, Push 21]
      expressionToStackCode "2+2;3-3;" `shouldBe` Right [Push 4, Push 0]

