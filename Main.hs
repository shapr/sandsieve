{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Main where

import           Control.Monad.Logger (LoggingT, runNoLoggingT, runStdoutLoggingT)
import           Control.Monad.Reader
import           Data.Attoparsec.Text hiding (take)
import           Data.Text hiding (filter, take)
import qualified Data.Text.IO as TIO
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           GHC.Generics
import           Control.Monad.Trans.Resource

import           Lib hiding (CPU, Instruction)

fromRight (Right x) = x
fromRight _         = error "you did it wrong"

files_to_read = ["A10-7860K", "i5-2467M-0x25", "Celeron-G540-0x29", "e3-1505M-0xa6"]

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Instruction
    v Text
    l Text
    s Text
    c Text

CPU
    vendor  Text
    family   Text
    model    Text
    modelname       Text
    stepping      Text
    microcode    Text
    deriving Show Generic

Upload
    cpu CPU
    instructions [Instruction]
|]

runDb = runStdoutLoggingT . runResourceT . withSqliteConn "sandsieve.db" . runSqlConn

main :: IO ()
main = do
  runDb $ runMigration migrateAll
  rows <- liftIO $ runDb $ selectList [] [Asc UploadId]
  contents <- TIO.readFile "A10-7860K"
  let results = parseOnly pWorks contents
      headers = parseOnly (many1 qHack) contents
      cleanheaders = filter (/= "\n") $ fromRight headers
  print (sixtuple cleanheaders)
  print . take 15 $ fromRight results


-- parseFile :: Text -> CPU
-- parseFile thefile =
--   let results = parseOnly pWorks contents
--       headers = parseOnly (many1 qHack) contents
--       cleanheaders = filter (/= "\n") $ fromRight header
--   in fromRight
