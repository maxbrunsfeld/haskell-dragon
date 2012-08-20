import Test.Hspec
import Ch2.Ex1

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

