{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Main where

import           Control.Applicative
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger (LoggingT, runNoLoggingT, runStdoutLoggingT)
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Attoparsec.Text hiding (take)
import           Data.Attoparsec.Text hiding (take)
import           Data.Char
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text hiding (filter, take)
import qualified Data.Text.IO as TIO
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           GHC.Generics

fromRight (Right x) = x
fromRight _         = error "you did it wrong"

sixtuple (a:b:c:d:e:f) = (a,b,c,d,e,f)

files_to_read = ["A10-7860K", "i5-2467M-0x25", "Celeron-G540-0x29", "e3-1505M-0xa6"]

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Instruction
    instruction  Text
    v            Text
    l            Text
    s            Text
    c            Text

CPU
    vendor  Text
    family   Text
    model    Text
    modelname       Text
    stepping      Text
    microcode    Text
    deriving Show Generic

Pair
    cpu CPU
    instructions Instruction
|]

runDb = runStdoutLoggingT . runResourceT . withSqliteConn "sandsieve.db" . runSqlConn

emptyInstruction = Instruction "" "" "" "" ""

emptyCPU = CPU "" "" "" "" "" ""

main :: IO ()
main = do
  runDb $ runMigration migrateAll
  rows <- liftIO $ runDb $ selectList [] [Asc PairId]
  contents <- TIO.readFile "Celeron-G540-0x29"
  let results = parseOnly pWorks contents
      headers = parseOnly (many1 qHack) contents
      realres = fromRight results
      cleanheaders = filter (/= "\n") $ fromRight headers
  print (sixtuple cleanheaders)
  -- _ <- runDb $ insert $ Pair emptyCPU
  _ <- runDb $ insert $ emptyCPU
  _ <- runDb $ insertMany realres
  print "whatever"


-- parseFile :: Text -> CPU
-- parseFile thefile =
--   let results = parseOnly pWorks contents
--       headers = parseOnly (many1 qHack) contents
--       cleanheaders = filter (/= "\n") $ fromRight header
--   in fromRight


alphaNums = takeWhile1 (\x -> isDigit x || isAlpha x || x == '(' || x == ')' )
-- how do we use the code above?
parsedLettersNumbers = parseOnly alphaNums "foo45 bar75"
failparsed = parseOnly alphaNums "$()$*"

spaces = many1 space

spaceAN = spaces *> alphaNums
-- spaceANa = spaces <*> alphaNums -- wtf shae?


parseBits = many1 spaceAN

-- parsedThings = parseOnly (many1 parseBits) ins1

-- this picks up five strings, the input has six, bet this fails against a bunch of lines
-- pInstruction :: Parser Instruction
pInstruction = Instruction <$> spaceAN <*> spaceAN <*> spaceAN <*> spaceAN <*> spaceAN

pManyIns = pInstruction <* (spaces *> alphaNums)

pLine = dropHeader *> pManyIns

-- data CPUData = Ins Instruction | Header Text

-- data CPU = CPU {
--   vendor      :: Text
--   , family    :: Text
--   , model     :: Text
--   , modelname :: Text
--   , stepping  :: Text
--   , microcode :: Text
--   } deriving (Show)

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

dropHeader :: Parser Text
dropHeader = string "#" *> takeTill (== '\n') *> string "\n"

pTillEnd = takeTill $ \x -> x == '\n'

-- pString s = string s *> alphaNums
pString s = string s *> pTillEnd <* string "\n"
pVendor = pString "# vendor_id\t: "
pFamily = pString "# cpu family\t: "
pModel = pString "# model\t\t: "
pModelName = pString "# model name\t: "
pStepping = pString "# stepping\t: "
pMicrocode = pString "# microcode\t: "

qHack = pVendor <|> pFamily <|> pModel <|> pModelName <|> pStepping <|> pMicrocode <|> dropHeader

pWorks = many dropHeader *> many1 pManyIns
