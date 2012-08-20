-- Convert decimal integer to Roman numeral

module Ch2.Ex1 where

import Data.List ( foldl' )
import Data.Char ( digitToInt )

decimalToRoman :: String -> String
decimalToRoman = integerToRoman . decimalToInteger
  where

    decimalToInteger = foldl' step 0
      where step n d = 10 * n + (digitToInt d)

    integerToRoman n =
      if n <= 0
        then ""
        else let (val, letter) = greatestPair n
             in letter : integerToRoman (n - val)

    greatestPair n =
      head . filter ((<= n) . fst) $ values

    values = [
      (100, 'C'),
      (50, 'L'),
      (10, 'X'),
      (5, 'V'),
      (1, 'I')]

