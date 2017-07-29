import TextGen (
  TextGen,
  Vocab,
  TextGenCh,
  runTextGen,
  word,
  aan,
  weighted,
  choose,
  list,
  randrep,
  perhaps,
  smartjoin,
  upcase,
  loadVocab
  )

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
-- A NUMBER of
--

-- TODO: have "a hundred", "a dozen" but "a score of"

number :: TextGenCh
number = list [ numbers, choose $ map word [ "score", "hundred", "dozen" ] ]

numbers :: TextGenCh
numbers = choose $ map word [
  "several",
  "two", "three", "four", "five", "six", "seven", "eight", "nine",
  "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
  "sixteen", "seventeen", "eighteen", "nineteen"
  ]


--
-- ADJECTIVE-PARTICIPLING WARRIORS
--

warriors :: Vocab -> TextGenCh
warriors v = list [ randrep ( 0, 2 ) epithet, v "heroes" ]
  where epithet = list [ v "hero_adj", word "-", v "hero_noun" ]


--
-- WHO DRINK the WATERS of the RIVER
--

whodrink :: Vocab -> TextGenCh
whodrink v = list [
  v "water_drink", v "water_adj", word "waters of the", v "waterways"
  ]

--
-- WAVING their WEAPONS
--
waving :: Vocab -> TextGenCh
waving v = list [
  v "flourish", perhaps ( 3, 5 ) $ word "their", v "weapons"
  ]

--
-- WEARING COLOURED GARMENTS
--

wearing :: Vocab -> TextGenCh
wearing v = list [ v "wearing", perhaps ( 1, 2 ) (v "colours"), v "clothes" ]

--
-- their ARMOUR SHINING like (a LOT of) SHINY THINGS
--

shining :: Vocab -> TextGenCh
shining v = list [ word "their", v "armour", v "shining", shinies ]
  where shinies = choose [ v "shinies", manyshinies ]
        manyshinies = aan $ list [ number, v "shinies_countable" ]
        number = choose $ map word [ "hundred", "thousand", "myriad" ] 


--
-- (ADVERBLY) SHOUTING ((ADJECTIVE) SOMETHING)
-- or SHOUTING (ADVERBLY)
--

shouting :: Vocab -> TextGenCh
shouting v = choose [ shout, shoutathing ]
  where shout = list [ v "shout_int", v "shout_adv" ]
        shoutathing = list [ madverb, v "shout", v "slogans" ]
        madverb = perhaps ( 1, 2 ) (v "shout_adv")
        
--
-- PLACES
--

place :: Vocab -> TextGenCh
place v = weighted [
  ( 2, v "places" ),
  ( 1, animal v ),
  ( 1, phenomena v ),
  ( 1, desert v ),
  ( 1, forest v ),
  ( 1, mountains v )
  ]

-- PLACE, ABODE of ANIMALS
-- PLACE, where they VERB the ANIMAL

animal :: Vocab -> TextGenCh
animal v = list [ v "places", phr $ choose [ abode, activity ] ]
  where abode = list [ v "abode", v "animals_plural" ]
        activity = list [
          word "where they", v "hunt", word "the", v "animals_sing"
          ]

-- PLACE, ABODE of THINGS

phenomena v = list [ v "places", phenoms ]
  where phenoms = phr $ list [ v "abode", v "phenomena" ]

-- the BOSKY SLIPPERY ELM GROVES of the FOREST
  
forest :: Vocab -> TextGenCh
forest v = list [
  word "the", adj, trees, v "groves", word "of the", v "forests"
  ]
  where adj = perhaps ( 1, 2 ) $ v "tree_adj"
        trees = perhaps ( 2, 3 ) $ v "trees"

-- the TOWERING STACKS of the MOUNTAIN RANGE

mountains :: Vocab -> TextGenCh
mountains v = list [ word "the", adj, v "peaks", word "of", v "mountains" ]
  where adj = perhaps ( 0, 1 ) $ v "mountain_adj"

-- the ACHING FLATS of the DESERT

desert :: Vocab -> TextGenCh
desert v = list [
  word "the", v "desert_adj", v "sands", word "of the", v "desert"
  ]

--
-- SENTENCES
--
-- From PLACE came N WARRIORS WHO (DRINK|CARRY|WEAR|SHINE)
-- PLACE sent N WARRIORS WHO (DRINK|CARRY|WEAR|SHINE)
-- N WARRIORS WHO (DRINK|CARRY|WEAR|SHINE) came from PLACE
-- (CARRY|WEAR)ING, N WARRIORS (WHO DRINK) arrived from PLACE 

host :: Vocab -> TextGenCh
host v = choose [ s1, s2, s3, s4 ]
  where s1 = list [ word "From", place v, word "came", band ]
        s2 = list [ place v, word "sent", band ] 
        s3 = list [ band, camefrom, place v ]
        s4 = list [ predo, number, warriors v, mwhodrink, camefrom, place v ]
        band = list [ number, warriors v, postdo ]
        postdo = perhaps ( 3, 5 ) $ choose $ map phr [
          whodrink v, waving v, wearing v, shining v, shouting v
          ]
        predo = perhaps ( 2, 5 ) $ choose $ map prephr [
          waving v, wearing v, shouting v, shining v
          ]
        mwhodrink = perhaps ( 2, 5 ) $ phr $ whodrink v
        camefrom = choose $ map word [ "came from", "arrived from" ]


phr phrase = list [ word ",", phrase, word "," ]

prephr phrase = list [ phrase, word "," ]
  
getDir (x:xs) = x
getDir _      = "./"


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
  bandf <- return $ runTextGen $ host v
  result <- iterateUntil (\s -> length s <= max_length) $ do 
    band <- getStdRandom bandf
    return $ upcase $ smartjoin band
  putStrLn result
