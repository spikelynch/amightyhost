import TextGen ( TextGen, runTextGen, word, choose, list, randrep, perhaps, smartjoin, upcase, loadOptions)

import Control.Monad (forM)
import Control.Monad.Loops (iterateUntil)
import Data.List (intercalate)
import Data.Char (toUpper)
import Text.Read (readMaybe)
import System.Random
import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as Tio

outfile = "hosts.txt"

nlines = 200

default_max_length :: Int
default_max_length = 140


--
-- ADJECTIVE-PARTICIPLING WARRIORS
--

warriors :: Vocab -> TextGenCh
warriors v = list [ randrep ( 0, 2 ) epithet, v "heroes" ]
  where epithet = list [ v "hero_adj", word "-", v "hero_noun" ]

--
-- PLACES
--

place :: Vocab -> TextGenCh
place v = weighted [
  ( 2, v "places" )
  ( 2, animal v )
  ( 1, desert v )
  ( 1, forest v )
  ( 1, mountains v )
  ]

-- PLACE, ABODE of ANIMALS
-- PLACE, where they VERB the ANIMAL

animal :: Vocab -> TextGenCh
animal v = list [ place, word ",", choose [ abode, activity ], word "," ]
  where abode = list [ v "abode", v "animals_plural" ]
        activity = list [
          word "where they", v "hunt", word "the", v "animals_sing"
          ]

-- the BOSKY SLIPPERY ELM GROVES of the FOREST
  
forest :: Vocab -> TextGenCh
forest v = list [
  word "the", adj, trees, v "groves", word "of the", v "forests"
  ]
  where adj = perhaps ( 1, 2 ) $ v "tree_adj"
        trees = perhaps ( 2, 3 ) $ v "tree"

-- the TOWERING STACKS of the MOUNTAIN RANGE

mountains :: Vocab -> TextGenCh
mountains v = list [ word "the", adj, v "peaks", word "of", v "mountains" ]
  where adj = perhaps ( 0, 1 ) $ v "mountain_adj"

-- the ACHING FLATS of the DESERT

desert :: Vocab -> TextGenCh
desert v = list [
  word "the", v "desert_adj", v "sands", word "of the", v "deserts"
  ]



trees ta t = list [ perhaps ( 1, 2 ) ta, t ]



whodrink wadj rivers = list [ word "who drink the", wadj, word "waters of the", rivers ]


flourish = choose $ map word [ "brandishing", "waving", "flourishing", "carrying", "armed with", "bearing", "humping", "hoisting", "loading", "tossing", "shouldering" ] 

wavingtheir v = list [
  v "flourish", perhaps (3, 5 ) $ word "their", v "weapons"
  ]

dressedin colours clothes = list [ participle, perhaps ( 1, 2 ) colours, clothes ]
  where participle = choose $ map word [ "wearing", "clad in", "dressed in", "sporting" ]


shininglike armour shinies = list [ word "their", armour, participle, shinies ]
  where participle = choose $ map word [ "shining like", "gleaming like", "glinting like", "glistening like" ]




numbers = choose $ map word [ "several", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen" ]

howmany = list [ numbers, choose $ map word [ "score", "hundred", "dozen" ] ]





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


maxLength :: [ String ] -> Int
maxLength (a:b:cs) = case readMaybe b of
  (Just i) -> i
  Nothing  -> default_max_length
maxLength _        = default_max_length




  
main :: IO ()
main = do
  args <- getArgs
  v <- loadVocab (getDir args)
  max_length <- return $ maxLength args
  bandf <- return $ runTextGen $ bandOfWarriors v
  result <- iterateUntil (\s -> length s <= max_length) $ do 
    band <- getStdRandom bandf
    return $ upcase $ smartjoin band
  putStrLn result
