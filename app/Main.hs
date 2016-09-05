import TextGen ( TextGen, runTextGen, word, choose, list, randrep, perhaps, smartjoin, upcase, loadOptions)

import Control.Monad (forM)
import Control.Monad.Loops (iterateUntil)
import Data.List (intercalate)
import Data.Char (toUpper)
import System.Random
import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as Tio

outfile = "hosts.txt"

nlines = 200
max_length = 140

abode a = list [ sof, a ] 
  where sof = choose $ map word [ "abode of", "haunt of", "country of", "land of", "home of", "place of", "realm of" ]


theyhunt a = list [ word "where they", anverb, word "the", a ]
  where anverb = choose $ map word [ "hunt", "eat", "fight", "worship", "dread" ]


trees ta t = list [ perhaps ( 1, 2 ) ta, t ]



whodrink wadj rivers = list [ word "who drink the", wadj, word "waters of the", rivers ]


flourish = choose $ map word [ "brandishing", "waving", "flourishing", "carrying", "armed with", "bearing", "humping", "hoisting", "loading", "tossing", "shouldering" ] 

wavingtheir weapons = list [ flourish, perhaps (3, 5 ) $ word "their", weapons ]

dressedin colours clothes = list [ participle, perhaps ( 1, 2 ) colours, clothes ]
  where participle = choose $ map word [ "wearing", "clad in", "dressed in", "sporting" ]


shininglike armour shinies = list [ word "their", armour, participle, shinies ]
  where participle = choose $ map word [ "shining like", "gleaming like", "glinting like", "glistening like" ]



topos_animal sing plural = list [ word ",", opts, word "," ]
  where opts = choose [ abode plural, abode plural, theyhunt sing ]

topos_forest tree tree_adj = list [ word "the", adj, tree, grove, word "of" ]
  where adj = perhaps ( 0, 1 ) tree_adj
        grove = choose $ map word [ "forests", "woods", "groves", "copses", "thickets", "wildwoods", "jungles" ]


numbers = choose $ map word [ "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen" ]

howmany = list [ numbers, choose $ map word [ "score", "hundred", "dozen" ] ]



epithet a n = randrep ( 0, 2 ) $ list [ a, word "-", n ]


-- From PLACE came N WARRIORS, DOING
-- PLACE sent N WARRIORS, DOING
-- N WARRIORS WHODRINK CAME FROM PLACE DOING

-- N WARRIORS DOING CAME FROM PLACE

-- DOING, WARRIORS, WHODRINK, CAME FROM PLACE

host place warriors waters arms dress simile = choose [ s1, s2, s3, s4 ]
  where s1 = list [ word "From", place, word "came", band ]
        s2 = list [ place, word "sent", band ] 
        s3 = list [ band, word "came from", place ]
        s4 = list [ action, howmany, warriors, attr, word "arrived from", place ]
        band = list [ howmany, warriors, whodo ]
        whodo = perhaps ( 3, 5 ) $ choose $ map phr [ waters, arms, dress, simile ]
        action = perhaps ( 2, 5 ) $ choose $ map prephr [ arms, dress ]
        attr = perhaps ( 2, 5 ) $ phr waters




phr phrase = list [ word ",", phrase, word "," ]

prephr phrase = list [ phrase, word "," ]
  
getDir (x:xs) = x
getDir _      = "./"

  
main :: IO ()
main = do
  args <- getArgs
  dataDir <- return $ getDir args
  places <- loadOptions (dataDir ++ "places.txt")
  water <- loadOptions (dataDir ++ "waterways.txt")
  wadj <- loadOptions (dataDir ++ "water_adj.txt")
  animal <- loadOptions (dataDir ++ "animals_sing.txt")
  animals <- loadOptions (dataDir ++ "animals_plural.txt")
  trees <- loadOptions (dataDir ++ "trees.txt")
  tree_adj <- loadOptions (dataDir ++ "tree_adj.txt")
  weapons <- loadOptions (dataDir ++ "weapons.txt")
  heroes <- loadOptions (dataDir ++ "heroes.txt")
  hero_adj <- loadOptions (dataDir ++ "hero_adj.txt")
  hero_noun <- loadOptions (dataDir ++ "hero_noun.txt")
  colours <- loadOptions (dataDir ++ "colours.txt")
  clothes <- loadOptions (dataDir ++ "clothes.txt")
  armour <- loadOptions (dataDir ++ "armour.txt")
  shining <- loadOptions (dataDir ++ "shining.txt")
  warriors <- return $ list [ epithet hero_adj hero_noun, heroes ]
  p_animal <- return $ list [ places, topos_animal animal animals ]
  p_forest <- return $ list [ topos_forest trees tree_adj, places ]
  topoi <- return $ choose [ places, p_animal, p_forest ]
  waters <- return $ whodrink wadj water
  armedwith <- return $ wavingtheir weapons
  dress <- return $ dressedin colours clothes
  shinelike <- return $ shininglike armour shining 
  bandf <- return $ runTextGen $ host topoi warriors waters armedwith dress shinelike
  result <- iterateUntil (\s -> length s <= max_length) $ do 
    band <- getStdRandom bandf
    return $ upcase $ smartjoin band
  putStrLn result
