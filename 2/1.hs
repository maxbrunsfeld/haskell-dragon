-- Convert decimal integer to Roman numeral

module Main where

import Test.Hspec
import Data.List ( foldl' )
import Data.Maybe ( fromJust )

decimalToRoman :: String -> String
decimalToRoman = integerToRoman . decimalToInteger
    where

      decimalToInteger = foldl' step 0
        where step n d = 10 * n + (fromJust $ lookup d decimals)

      integerToRoman n =
        let (val, letter) = greatestPair n
        in if n <= 0
           then ""
           else letter : integerToRoman (n - val)

      greatestPair n =
        head . filter ((<= n) . fst) $ values

      decimals = [
        ('0', 0),
        ('1', 1),
        ('2', 2),
        ('3', 3),
        ('4', 4),
        ('5', 5),
        ('6', 6),
        ('7', 7),
        ('8', 8),
        ('9', 9)]

      values = [
        (100, 'C'),
        (50, 'L'),
        (10, 'X'),
        (5, 'V'),
        (1, 'I')]

main = hspec $
  describe "decimalToRoman" $ do
    it "works for numbers <= 5" $ do
      decimalToRoman "2" `shouldBe` "II"
      decimalToRoman "3" `shouldBe` "III"

    it "works for numbers <= 10" $ do
      decimalToRoman "7" `shouldBe` "VII"
      decimalToRoman "8" `shouldBe` "VIII"

    it "works for numbers <= 50" $ do
      decimalToRoman "12" `shouldBe` "XII"
      decimalToRoman "33" `shouldBe` "XXXIII"

    it "works for numbers <= 100" $ do
      decimalToRoman "74" `shouldBe` "LXXIIII"
      decimalToRoman "51" `shouldBe` "LI"

    it "works for numbers <= 500" $ do
      decimalToRoman "374" `shouldBe` "CCCLXXIIII"
      decimalToRoman "101" `shouldBe` "CI"

