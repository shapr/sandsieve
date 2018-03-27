{-# LANGUAGE OverloadedStrings          #-}
module Lib where

import Control.Applicative
import Data.Attoparsec.Text hiding (take)
import qualified Data.Text as T
import Data.Char
import Data.Text (Text)

ins1,ins2,ins3 :: Text
ins1 = "                      0f0d04cf  1  4  5  2 (0f0d04cf000000000000000000000000)"
ins2 = "              0f0d04cdff000000  1  8  5  2 (0f0d04cdff0000000000000000000000)"
ins3 = "                        0f0d00  1  3  5  2 (0f0d0000000000000000000000000000)"
ins4 = T.concat ["#stuff","\n",ins1,"\n",ins2]

data Instruction = Instruction {
  instruction :: Text
  , v         :: Text
  , l         :: Text
  , s         :: Text
  , c         :: Text
  } deriving (Show, Eq)

-- code below documented at https://hackage.haskell.org/package/attoparsec-0.13.2.2/docs/Data-Attoparsec-Text.html

dropHeader = char '#' *> takeTill (== '\n') *> char '\n'
alphaNums = takeWhile1 (\x -> isDigit x || isAlpha x || x == '(' || x == ')' )
-- how do we use the code above?
parsedLettersNumbers = parseOnly alphaNums "foo45 bar75"
failparsed = parseOnly alphaNums "$()$*"

spaces = many1 space

spaceAN = spaces *> alphaNums
-- spaceANa = spaces <*> alphaNums -- wtf shae?


parseBits = many1 spaceAN

parsedThings = parseOnly (many1 parseBits) ins1

-- this picks up five strings, the input has six, bet this fails against a bunch of lines
pInstruction :: Parser Instruction
pInstruction = Instruction <$> spaceAN <*> spaceAN <*> spaceAN <*> spaceAN <*> spaceAN

pManyIns = pInstruction <* (spaces *> alphaNums)

pLine = dropHeader *> pManyIns

data CPUData = Ins Instruction | Header Text

data CPU = CPU {
  vendor      :: Text
  , family    :: Text
  , model     :: Text
  , modelname :: Text
  , stepping  :: Text
  , microcode :: Text
  } deriving (Show)

headerText :: Text
headerText =
  "# cpu:\
  \# processor\t: 0\
  \# vendor_id\t: GenuineIntel\n\
  \# cpu family\t: 6\n\
  \# model\t\t: 42\n\
  \# model name\t: Intel(R) Celeron(R) CPU G540 @ 2.50GHz\n\
  \# stepping\t: 7\n\
  \# microcode\t: 0x29\n"

pTillEnd = takeTill $ \x -> x == '\n'

-- pString s = string s *> alphaNums
-- lsp-rename
pString s = string s *> pTillEnd
pVendor = pString "# vendor_id\t: "
pFamily = pString "# cpu family\t: "
pModel = pString "# model\t\t: "
pModelName = pString "# model name\t: "
pStepping = pString "# stepping\t: "
pMicrocode = pString "# microcode\t: 0x29"

-- qHack :: Parser CPUData
qHack = pVendor *> pFamily <|> pModel <|> pModelName <|> pStepping <|> pMicrocode

pWorks = many dropHeader *> many1 pManyIns
