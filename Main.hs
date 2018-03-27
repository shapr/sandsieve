{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Attoparsec.Text hiding (take)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Lib

main :: IO ()
main =
  do contents <- TIO.readFile "Celeron-G540-0x29"
     let results = parseOnly pWorks contents
         headers = parseOnly (many1 qHack) contents
         cleanheaders = filter (/= "\n") $ fromRight headers
     print cleanheaders
     print . take 15 $ fromRight results

fromRight (Right x) = x
fromRight _         = error "you did it wrong"
