-- Convert an arithmetic expression to instructions
-- for an abstract stack machine

module Main where

import Test.Hspec
import Control.Monad.State

expressionToStackCode :: String -> [Instruction]
expressionToStackCode s = evalState action []
  where
    action = do
      get
      return [Push 5]

-- Primitive instructions for an abstract state machine
data Instruction =
  Push Int |
  RValue Location |
  LValue Location |
  Pop |
  Assign |
  Copy
  deriving (Show, Eq)

type Location = String

main = hspec $
  describe "expressionToStackCode" $

    it "works for integers" $ do
      expressionToStackCode "5" `shouldBe` [Push 5]
      expressionToStackCode "6" `shouldBe` [Push 6]
