{-# LANGUAGE OverloadedStrings #-}

module ProfScrapeLib
    (  numProfessors
    ) where

import Data.List
import Data.Maybe 
import Text.HTML.Scalpel

urlStub = "https://www.gla.ac.uk/schools/"
staffExt = "/staff"

numProfessors :: String -> IO (Maybe Int)
numProfessors x = do 
        profList <- scrapeStaff x
        return (countProfessors profList)

countProfessors :: Maybe [[String]] -> Maybe Int
countProfessors x = case x of
    Just str -> Just (length (filterTrueProfs (filterAllProfs (concat str))))
    Nothing -> Nothing

scrapeStaff :: String -> IO (Maybe [[String]]) 
scrapeStaff department = scrapeURL (getURL department) staffScraper
    where 
        staffScraper = chroot ("div" @: ["id" @= "content_1234567"]) scrapeNotHonorary

scrapeNotHonorary :: Scraper String [[String]]
scrapeNotHonorary = chroots ("ul" @: [notP ("id" @= "honorary-visitinglist")]) (texts "li")

filterAllProfs :: [String] -> [String]
filterAllProfs = filter ("Professor" `isInfixOf`) 

filterTrueProfs :: [String] -> [String]
filterTrueProfs = filter (\p -> not (("Assistant" `isInfixOf` p) || ("Associate" `isInfixOf` p)))

getURL :: String -> String
getURL s =  urlStub ++ s ++ staffExt

