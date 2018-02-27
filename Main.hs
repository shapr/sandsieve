{-# LANGUAGE OverloadedStrings          #-}
module Main where

import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char

main :: IO ()
main = putStrLn "Hello, Haskell!"


headerText :: Text
headerText =
  "# cpu:\
  \# processor\t: 0\
  \# vendor_id\t: GenuineIntel\
  \# cpu family\t: 6\
  \# model\t\t: 42\
  \# model name\t: Intel(R) Celeron(R) CPU G540 @ 2.50GHz\
  \# stepping\t: 7\
  \# microcode\t: 0x29"

ins1,ins2,ins3 :: Text
ins1 = "                      0f0d04cf  1  4  5  2 (0f0d04cf000000000000000000000000)"
ins2 = "              0f0d04cdff000000  1  8  5  2 (0f0d04cdff0000000000000000000000)"
ins3 = "                        0f0d00  1  3  5  2 (0f0d0000000000000000000000000000)"

data CPU = CPU {
  vendor :: Text
  , family :: Text
  , model :: Text
  , modelname :: Text
  , stepping :: Text
  , microcode :: Text
  } deriving (Show)

data Instruction = Instruction {
  instruction :: Text
  , v :: Text
  , l :: Text
  , s :: Text
  , c :: Text
  } deriving (Show)


alphaNums = takeWhile1 (\x -> isDigit x || isAlpha x || x=='(' || x==')')
-- how do we use the code above?
parsedLettersNumbers = parseOnly alphaNums "foo45 bar75"
failparsed = parseOnly alphaNums "$()$*"

spaces = many1 space

spaceAN = spaces *> alphaNums

parseBits = many1 spaceAN

parsedThings = parseOnly (many1 parseBits) ins1

-- this picks up five strings, the input has six, bet this fails against a bunch of lines
pInstruction :: Parser Instruction
pInstruction = Instruction <$> spaceAN <*> spaceAN <*> spaceAN <*> spaceAN <*> spaceAN
