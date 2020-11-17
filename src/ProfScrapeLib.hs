{-# LANGUAGE OverloadedStrings #-}

module ProfScrapeLib
    (  numProfessors
    ) where

import Text.HTML.Scalpel
import Data.List

numProfessors :: String -> IO (Maybe Int)
numProfessors [] = return Nothing
numProfessors school = do
    names <- scrapeURL (constructUrl school) scrapeList
    return (countProfessors names)

constructUrl :: String -> String
constructUrl school = "https://www.gla.ac.uk/schools/" ++ school ++ "/staff/"

scrapeList :: Scraper String [String]
scrapeList = chroot ("ul" @: ["id" @= "research-teachinglist"]) scrapeNames

scrapeNames :: Scraper String [String]
scrapeNames = texts "a"

countProfessors :: Maybe [String] -> Maybe Int
countProfessors Nothing = Nothing
countProfessors (Just names) = Just (length (filter (\n -> isInfixOf "Professor" n) names))