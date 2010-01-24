{-# LANGUAGE BangPatterns #-}

import Data.Char (toLower)
import Data.Map (Map, fromListWith, member, insertWith', keysSet, empty, findWithDefault)
import qualified Data.Map as Map (empty)
import Data.Set as Set (Set, fromList, toList, fold, null) 
import Data.List (inits, tails, foldl')
import Data.List.Split (wordsBy)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
-- import Test.QuickCheck

type WordFreq = Map String Int
type WordSet = Set String

dataFile = "big.txt"
alphabet = "abcdefghijklmnopqrstuvwxyz"

splitWords :: String -> [String]
splitWords = wordsBy (\c -> c < 'a' || c > 'z') . map toLower

train :: [String] -> WordFreq
train = foldl' updateMap Map.empty 
  where updateMap model word = insertWith' (+) word 1 model

myReadFile = readFile dataFile

nwords :: IO WordFreq
nwords = return . train . splitWords =<< myReadFile

edits1 :: String -> [String]
edits1 s = toList $ fromList $ deletes ++ transposes ++ replaces ++ inserts
  where
    deletes    = [a ++ bs | (a, _:bs) <- splits]
    transposes = [a ++ (b2:b1:bs) | (a, b1:b2:bs) <- splits]
    replaces   = [a ++ (c:bs) | (a, _:bs) <- splits, c <- alphabet]
    inserts    = [a ++ (c:b) | (a, b) <- splits, c <- alphabet]
    splits     = zip (inits s) (tails s)

correct :: WordFreq -> String -> String
correct wordCounts word = fst $ fold maxCount ("?", 0) candidates
  where
    candidates :: WordSet
    candidates = 
      known [word] `or` (known $ edits1 word) `or` known_edits2 word

    known_edits2 :: String -> WordSet
    known_edits2 w =
      fromList [w2 | w1 <- edits1 w, w2 <- edits1 w1, w2 `member` wordCounts]

    known :: [String] -> WordSet
    known ws = fromList [w | w <- ws, w `member` wordCounts]
    
    maxCount :: String -> (String, Int) -> (String, Int)
    maxCount word current@(_, currentMax) 
      | count > currentMax = (word, count)
      | otherwise          = current
      where count = findWithDefault 1 word wordCounts

    or :: WordSet -> WordSet -> WordSet
    or a b | Set.null a = b
           | otherwise  = a

main :: IO ()
main = do 
  args <- getArgs
  wordCounts <- nwords
  mapM_ (printCorrect wordCounts) args
  where
    printCorrect :: WordFreq -> String -> IO ()
    printCorrect wordCounts word =
      putStrLn $ word ++ " -> " ++ correct wordCounts word

-- Testing --

-- instance Arbitrary Char where
--   arbitrary     = frequency [(4, choose ('\33', '\128')), (1, return ' ')]
--   coarbitrary c = variant (ord c `rem` 4)

-- prop_words_nospaces s = all (not . elem ' ') (splitWords s)

-- prop_words_noempty s = all ((> 0) . length) (splitWords s)
