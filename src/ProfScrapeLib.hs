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

-- Helper function to create the correct url
constructUrl :: String -> String
constructUrl school = "https://www.gla.ac.uk/schools/" ++ school ++ "/staff/"

-- Finds the "tabs" div on the page - if this div isn't present then we 
-- know it's not a valid url to scrape
scrapeList :: Scraper String [[String]]
scrapeList = chroot ("div" @: ["id" @= "tabs"]) scrapeNames

-- Selects the text in all anchor elements (names) found within the list, 
-- ignoring the honorary/visiting section.
-- We only care if they have Professor in their name, we don't care about job title.
scrapeNames :: Scraper String [[String]]
scrapeNames = chroots ("ul" @: [notP ("id" @= "honorary-visitinglist")]) (texts "a")

-- Helper function: Counts the appearances of Professor in the list of names
countProfessors :: [String] -> Int
countProfessors [] = 0
countProfessors names = length (filter (\n -> isInfixOf "Professor" n) names)

-- Counts total number of Professors in all lists using a fold, 
-- propogates the Nothing value, which is passed in if a wrong url is loaded.
countProfessorsList :: Maybe [[String]] -> Maybe Int
countProfessorsList Nothing = Nothing
countProfessorsList (Just names) = Just (foldl (\x y -> x + (countProfessors y)) 0 names )