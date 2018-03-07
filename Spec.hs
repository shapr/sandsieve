{-# LANGUAGE OverloadedStrings          #-}
module Main where

import Test.Hspec
import Data.Text
import Lib
import Data.Attoparsec.Text

main = hspec $ do
  describe "test description" $ do
    context "some context" $ do
      it "does a thing" $
        (1 + 1) `shouldBe` 2
  describe "parse instructions" $ do
    context "given an instruction" $ do
      it "parses successfully" $
        (parseOnly pInstruction ins1) `shouldBe` Right (Instruction "0f0d04cf" "1" "4" "5" "2")

-- ins1,ins2,ins3 :: Text
-- ins1 = "                      0f0d04cf  1  4  5  2 (0f0d04cf000000000000000000000000)"
-- ins2 = "              0f0d04cdff000000  1  8  5  2 (0f0d04cdff0000000000000000000000)"
-- ins3 = "                        0f0d00  1  3  5  2 (0f0d0000000000000000000000000000)"
