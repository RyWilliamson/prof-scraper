{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Text.HTML.Scalpel
import ProfScrapeLib
import Data.Csv
import GHC.Generics
import qualified Data.Vector as Vector
import qualified Data.ByteString.Lazy as ByteString

schools :: [String]
schools = [
          "chemistry",
          "computing",
          "engineering",
          "ges",
          "mathematicsstatistics",
          "physics",
          "psychology"
         ]

data Info = Info { school :: !String, professor :: !(Maybe Int) } deriving (Generic, Show)

instance FromNamedRecord Info
instance ToNamedRecord Info
instance DefaultOrdered Info

resultsToRecords :: [(String, Maybe Int)] -> [Info]
resultsToRecords results = map (\x -> Info (fst x) (snd x)) results

main :: IO ()
main = do
     counts <- mapM numProfessors schools
     let results = zip schools counts
     mapM_ (\x -> putStrLn $ (fst x) ++ ": " ++ ((show.snd) x)) results
     let records = resultsToRecords results
     ByteString.writeFile "output.csv" (encodeDefaultOrderedByName records)

