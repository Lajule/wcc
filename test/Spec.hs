import Test.Hspec
import WCC (File(..), parse)

main :: IO ()
main = hspec $ do
  describe "parse" $ do
    it "can parse" $ do
      let f = parse "foo" "bar"
      filepath f `shouldBe` "foo"
      bytes f `shouldBe` 3
      newlines f `shouldBe` 1
