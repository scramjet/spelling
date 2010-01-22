{-# LANGUAGE BangPatterns #-}

import Data.Char (toLower)
import Data.Map (Map, fromListWith, insertWith', keysSet, empty)
import qualified Data.Map as Map (lookup, empty)
import Data.Set as Set (Set, fromList, toList, member, fold, null) 
import Data.List (inits, tails, foldl')
import Data.List.Split (wordsBy)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
-- import Test.QuickCheck

dataFile = "big.txt"
alphabet = "abcdefghijklmnopqrstuvwxyz"

splitWords :: String -> [String]
splitWords = wordsBy (\c -> c < 'a' || c > 'z') . map toLower

train :: [String] -> Map String Int
train = foldl' updateMap Map.empty 
  where updateMap model word = insertWith' (+) word 1 model

nwords :: IO (Map String Int)
nwords = return . train . splitWords =<< readFile dataFile

edits1 :: String -> [String]
edits1 s = toList $ fromList $ deletes ++ transposes ++ replaces ++ inserts
  where
    deletes    = [a ++ bs | (a, _:bs) <- splits]
    transposes = [a ++ (b2:b1:bs) | (a, b1:b2:bs) <- splits]
    replaces   = [a ++ (c:bs) | (a, _:bs) <- splits, c <- alphabet]
    inserts    = [a ++ (c:b) | (a, b) <- splits, c <- alphabet]
    splits     = zip (inits s) (tails s)

correct :: Map String Int -> String -> String
correct wordCounts word = fst $ fold maxCount ("?", 0) candidates
  where
    candidates :: Set String
    candidates = 
      known [word] `or` (known $ edits1 word) `or` known_edits2 word

    known_edits2 :: String -> Set String
    known_edits2 w =
      fromList [w2 | w1 <- edits1 w, w2 <- edits1 w1, w2 `member` allWords]

    allWords :: Set String
    allWords = keysSet wordCounts

    known :: [String] -> Set String
    known ws = fromList [w | w <- ws, w `member` allWords]
    
    maxCount :: String -> (String, Int) -> (String, Int)
    maxCount word current@(_, currentMax) 
      | count > currentMax = (word, count)
      | otherwise          = current
      where count = fromMaybe 1 (Map.lookup word wordCounts)

    or :: Set String -> Set String -> Set String
    or a b | Set.null a = b
           | otherwise  = a

main :: IO ()
main = do 
  args <- getArgs
  wordCounts <- nwords
  mapM_ (printCorrect wordCounts) args
  where
    printCorrect :: Map String Int -> String -> IO ()
    printCorrect wordCounts word =
      putStrLn $ word ++ " -> " ++ correct wordCounts word

-- Testing --

-- instance Arbitrary Char where
--   arbitrary     = frequency [(4, choose ('\33', '\128')), (1, return ' ')]
--   coarbitrary c = variant (ord c `rem` 4)

-- prop_words_nospaces s = all (not . elem ' ') (splitWords s)

-- prop_words_noempty s = all ((> 0) . length) (splitWords s)
