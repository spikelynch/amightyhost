module Main where 

import TextGen ( TextGen, runTextGen, word, choose, list, randrep, perhaps, smartjoin, upcase, loadOptions)

import Control.Monad (forM)
import Data.List (intercalate)
import Data.Char (toUpper)
import System.Random
import qualified Data.Text as T
import qualified Data.Text.IO as Tio

outfile = "hosts.txt"

nlines = 200


abode a = list [ sof, a ] 
  where sof = choose $ map word [ "abode of", "haunt of", "country of", "land of", "home of", "place of", "realm of" ]


theyhunt a = list [ word "where they", anverb, word "the", a ]
  where anverb = choose $ map word [ "hunt", "eat", "fight", "worship", "dread" ]
        


whodrink wadj rivers = list [ word "who drink the", wadj, word "waters of the", rivers ]


flourish = choose $ map word [ "brandishing", "waving", "flourishing", "carrying", "armed with", "bearing", "humping", "hoisting", "loading", "tossing", "shouldering" ] 

wavingtheir weapons = list [ flourish, perhaps (3, 5 ) $ word "their", weapons ]

topos p sing plural = list [ p, randrep (0, 1) adjecclause ]
  where adjecclause = list [ word ",", opts, word "," ]
        opts = choose [ abode plural, abode plural, theyhunt sing ]


numbers = choose $ map word [ "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen" ]

howmany = list [ numbers, choose $ map word [ "score", "hundred", "dozen" ] ]



epithet a n = randrep ( 0, 2 ) $ list [ a, word "-", n ]


host place warriors waters arms = choose [ f1, f2, f3 ]
  where f1 = list [ word "From", place, word "came", howmany, warriors, eitherattr ]
        f2 = list [ place, word "sent", howmany, warriors, eitherattr ] 
        f3 = list [ howmany, warriors, mwaters, word "came from", place, marms ]
        mwaters = perhaps ( 1, 3 ) $ wphrase 
        marms = perhaps ( 1, 3 ) $ aphrase
        eitherattr = perhaps ( 2, 5 ) $ choose [ wphrase, aphrase ]
        wphrase = phr waters
        aphrase = phr arms


phr phrase = list [ word ",", phrase, word "," ]
  
  
main :: IO ()
main = do   
  places <- loadOptions "data/places.txt"
  water <- loadOptions "data/waterways.txt"
  wadj <- loadOptions "data/water_adj.txt"
  animal <- loadOptions "data/animals_sing.txt"
  animals <- loadOptions "data/animals_plural.txt"
  weapons <- loadOptions "data/weapons.txt"
  heroes <- loadOptions "data/heroes.txt"
  hero_adj <- loadOptions "data/hero_adj.txt"
  hero_noun <- loadOptions "data/hero_noun.txt"
  warriors <- return $ list [ epithet hero_adj hero_noun, heroes ]
  topoi <- return $ topos places animal animals
  waters <- return $ whodrink wadj water
  armedwith <- return $ wavingtheir weapons
  nf <- return $ runTextGen $ host topoi warriors waters armedwith
  outlines <- forM [1..nlines] $ \i -> do 
    band <- getStdRandom nf
    return $ upcase $ smartjoin band
  fout <- return $ filter (\x -> length x < 141 ) outlines
  Tio.writeFile outfile $ T.intercalate (T.pack "\n") $ map T.pack fout
