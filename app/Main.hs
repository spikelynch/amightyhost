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
        


whodrink wadj rivers = list [ word "who drink the", wadj, word "waters of the", rivers ]


flourish = choose $ map word [ "brandishing", "waving", "flourishing", "carrying", "armed with", "bearing", "humping", "hoisting", "loading", "tossing", "shouldering" ] 

wavingtheir weapons = list [ flourish, perhaps (3, 5 ) $ word "their", weapons ]

dressedin colours clothes = list [ participle, perhaps ( 1, 2 ) colours, clothes ]
  where participle = choose $ map word [ "wearing", "clad in", "dressed in" ]


shininglike armour shinies = list [ word "their", armour, participle, shinies ]
  where participle = choose $ map word [ "shining like", "gleaming like", "glinting like", "glistening like" ]

topos p sing plural = list [ p, randrep (0, 1) adjecclause ]
  where adjecclause = list [ word ",", opts, word "," ]
        opts = choose [ abode plural, abode plural, theyhunt sing ]


numbers = choose $ map word [ "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen" ]

howmany = list [ numbers, choose $ map word [ "score", "hundred", "dozen" ] ]



epithet a n = randrep ( 0, 2 ) $ list [ a, word "-", n ]


host place warriors waters arms dress simile = choose [ f1, f2, f3 ]
  where f1 = list [ word "From", place, word "came", howmany, warriors, eitherattr ]
        f2 = list [ place, word "sent", howmany, warriors, eitherattr ] 
        f3 = list [ howmany, warriors, mwaters, word "came from", place, eitherattr ]
        mwaters = perhaps ( 1, 3 ) $ phr waters 
        marms = perhaps ( 1, 3 ) $ phr arms
        mdress = perhaps ( 1, 3 ) $ phr dress
        msimile = perhaps ( 1, 3 ) $ phr simile
        eitherattr = perhaps ( 2, 5 ) $ choose [ phr waters, phr arms, phr dress, phr simile ]


phr phrase = list [ word ",", phrase, word "," ]
  
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
  weapons <- loadOptions (dataDir ++ "weapons.txt")
  heroes <- loadOptions (dataDir ++ "heroes.txt")
  hero_adj <- loadOptions (dataDir ++ "hero_adj.txt")
  hero_noun <- loadOptions (dataDir ++ "hero_noun.txt")
  colours <- loadOptions (dataDir ++ "colours.txt")
  clothes <- loadOptions (dataDir ++ "clothes.txt")
  armour <- loadOptions (dataDir ++ "armour.txt")
  shining <- loadOptions (dataDir ++ "shining.txt")
  warriors <- return $ list [ epithet hero_adj hero_noun, heroes ]
  topoi <- return $ topos places animal animals
  waters <- return $ whodrink wadj water
  armedwith <- return $ wavingtheir weapons
  dress <- return $ dressedin colours clothes
  shinelike <- return $ shininglike armour shining 
  bandf <- return $ runTextGen $ host topoi warriors waters armedwith dress shinelike
  result <- iterateUntil (\s -> length s <= max_length) $ do 
    band <- getStdRandom bandf
    return $ upcase $ smartjoin band
  putStrLn result
