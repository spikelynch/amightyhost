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

topos_forest tree tree_adj grove = list [ word "the", adj, trees, grove, word "of the" ]
  where adj = perhaps ( 0, 1 ) tree_adj
        trees = perhaps ( 2, 3 ) tree


topos_mountains m m_adj peaks = list [ word "the", adj, peaks, word "of", m ]
  where adj = perhaps ( 0, 1 ) m_adj


topos_desert d d_adj d_sands = list [ word "the", d_adj, d_sands, word "of the", d ]

numbers = choose $ map word [ "several", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen" ]

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

load d file = loadOptions (d ++ file)
  
main :: IO ()
main = do
  args <- getArgs
  load <- return $ (\f -> loadOptions ((getDir args) ++ f))
  places <- load "places.txt"
  water <- load "waterways.txt"
  wadj <- load "water_adj.txt"
  animal <- load "animals_sing.txt"
  animals <- load "animals_plural.txt"
  trees <- load "trees.txt"
  tree_adj <- load "tree_adj.txt"
  forests <- load "forests.txt"
  groves <- load "groves.txt"
  mountains <- load "mountains.txt"
  mountain_adj <- load "mountain_adj.txt"
  peaks <- load "peaks.txt"
  deserts <- load "desert.txt"
  desert_adj <- load "desert_adj.txt"
  sands <- load "sands.txt"
  weapons <- load "weapons.txt"
  heroes <- load "heroes.txt"
  hero_adj <- load "hero_adj.txt"
  hero_noun <- load "hero_noun.txt"
  colours <- load "colours.txt"
  clothes <- load "clothes.txt"
  armour <- load "armour.txt"
  shining <- load "shining.txt"
  warriors <- return $ list [ epithet hero_adj hero_noun, heroes ]
  p_animal <- return $ list [ places, topos_animal animal animals ]
  p_forest <- return $ list [ topos_forest trees tree_adj groves, forests ]
  p_mountains <- return $ topos_mountains mountains mountain_adj peaks
  p_desert <- return $ topos_desert deserts desert_adj sands
  topoi <- return $ choose [ places, places, p_animal, p_animal, p_desert, p_forest, p_mountains ]
  waters <- return $ whodrink wadj water
  armedwith <- return $ wavingtheir weapons
  dress <- return $ dressedin colours clothes
  shinelike <- return $ shininglike armour shining 
  bandf <- return $ runTextGen $ host topoi warriors waters armedwith dress shinelike
  result <- iterateUntil (\s -> length s <= max_length) $ do 
    band <- getStdRandom bandf
    return $ upcase $ smartjoin band
  putStrLn result
