module Main where

import MathExpression
  ( eval,
    tokenizer,
  )
import Test.Hspec (Spec, it, shouldBe)
import Test.Hspec.Runner (defaultConfig, hspecWith)

main :: IO ()
main = hspecWith defaultConfig specs

specs :: Spec
specs = do
  it "basic arithmetic" $ do
    eval (tokenizer "1 + 2") `shouldBe` 3
    eval (tokenizer "3 - 2") `shouldBe` 1
    eval (tokenizer "2 * 2") `shouldBe` 4
    eval (tokenizer "4 / 2") `shouldBe` 2
    eval (tokenizer "1 + 2 * 3") `shouldBe` 7
