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
    return (countProfessorsList names)

constructUrl :: String -> String
constructUrl school = "https://www.gla.ac.uk/schools/" ++ school ++ "/staff/"

scrapeList :: Scraper String [[String]]
scrapeList = chroot ("div" @: ["id" @= "tabs"]) scrapeNames

scrapeNames :: Scraper String [[String]]
scrapeNames = chroots ("ul" @: [notP ("id" @= "honorary-visitinglist")]) (texts "a")

countProfessors :: [String] -> Int
countProfessors [] = 0
countProfessors names = length (filter (\n -> isInfixOf "Professor" n) names)

countProfessorsList :: Maybe [[String]] -> Maybe Int
countProfessorsList Nothing = Nothing
countProfessorsList (Just names) = Just (foldl (\x y -> x + (countProfessors y)) 0 names )