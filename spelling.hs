{-# LANGUAGE BangPatterns #-}

import Data.Char (toLower, ord)
import Data.Map (Map, fromListWith, keysSet)
import qualified Data.Map as Map (fromList, lookup)
import Data.Set as Set (Set, fromList, toList, member, fold, null) 
import Data.List (inits, tails)
import Data.List.Split (wordsBy)
import Data.Maybe (fromMaybe)
import Control.Monad (mapM, liftM)
import System.Environment (getArgs, withArgs)
-- import Test.QuickCheck

dataFile = "big.txt"
alphabet = "abcdefghijklmnopqrstuvwxyz"

splitWords :: String -> [String]
splitWords = wordsBy (\c -> c < 'a' || c > 'z') . map toLower

train :: [String] -> Map String Int
train = fromListWith (+) . map (\s -> (s, 1))

nwords :: IO (Map String Int)
nwords = readFile dataFile >>= return . train . splitWords

edits1 :: String -> Set String
edits1 s = fromList (deletes ++ transposes ++ replaces ++ inserts)
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
      known [word] `or` (known $ edits word) `or` known_edits2 word

    known_edits2 :: String -> Set String
    known_edits2 w =
      fromList [e2 | e1 <- edits w, e2 <- edits e1, e2 `member` allWords]

    edits :: String -> [String]
    edits = toList . edits1

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
  knownWords <- nwords
  mapM_ (correctWord knownWords) args
  where
    correctWord :: (Map String Int) -> String -> IO ()
    correctWord knownWords word = do
      (return $ correct knownWords word) >>= putStrLn

-- Testing --

-- instance Arbitrary Char where
--   arbitrary     = frequency [(4, choose ('\33', '\128')), (1, return ' ')]
--   coarbitrary c = variant (ord c `rem` 4)

-- prop_words_nospaces s = all (not . elem ' ') (splitWords s)

-- prop_words_noempty s = all ((> 0) . length) (splitWords s)
