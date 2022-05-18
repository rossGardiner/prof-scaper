module Main where

import Text.HTML.Scalpel
import Data.Maybe
import ProfScrapeLib 
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams


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


makeChart :: [Maybe Int] -> Renderable ()
makeChart x = toRenderable layout where
     
     counts = map (fromIntegral . fromMaybe 0) x
     values = zip schools counts
     
     pitem (s,v) = pitem_label .~ s
                  $ pitem_value .~ v
                  $ def

     layout = pie_title .~ "Professors in Each Dept."
            $ pie_plot . pie_data .~ map pitem values 
            $ def 

main :: IO ()
main = do 
     counts <- mapM numProfessors schools
     let chart = makeChart counts 
     renderableToFile def "../output.svg" chart
     return ()

