import Test.Hspec
import Ch2.Ex2

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

