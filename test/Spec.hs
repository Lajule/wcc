import Test.Hspec
import WCC as W

main :: IO ()
main = hspec $ do
  describe "parse" $ do
    it "can parse" $ do
      let f = W.parse "foo" "bar bar"
      W.filepath f `shouldBe` "foo"
      W.bytes f `shouldBe` 7
      W.words f `shouldBe` 2
      W.newlines f `shouldBe` 1
      W.word (W.most_repeated_word f) `shouldBe` "bar"
      W.word_count (W.most_repeated_word f) `shouldBe` 2
