import Data.Char (toLower, ord)
import Data.Map (Map, fromListWith, keysSet)
import qualified Data.Map as Map (fromList, lookup)
import Data.Set (Set, fromList, toList, member, union, fold)
import Data.List (inits, tails)
import Data.List.Split (wordsBy)
import Data.Maybe (fromMaybe)
import Test.QuickCheck
import Control.Monad (mapM, liftM)
import System.Environment (getArgs, withArgs)

dataFile = "big.txt"
alphabet = "abcdefghijklmnopqrstuvwxyz"

set :: [String] -> [String]
set = toList . fromList

splitWords :: String -> [String]
splitWords = wordsBy (\c -> c < 'a' || c > 'z') . map toLower

-- splitWords = splitRegex boundary . map toLower
--   where boundary = mkRegexWithOpts "[^a-z]+" False True

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

--     splits :: String -> [(String, String)]
--     splits = splits' ""
--       where
--         splits' a b@(hd:tl) = (a, b) : splits' (a ++ [hd]) tl
--         splits' h "" = [(h, "")]

known_edits2 :: String -> IO (Set String)
known_edits2 s = do
  let edits = toList . edits1
  knownWords <- nwords >>= return . keysSet
  return $ 
    fromList [e2 | e1 <- edits s, e2 <- edits e1, e2 `member` knownWords]

known :: [String] -> IO (Set String)
known ws = do
  knownWords <- nwords >>= return . keysSet
  return $ fromList [w | w <- ws, w `member` knownWords]

correct :: String -> IO String
correct word = do
  knownWords <- nwords
  first <- known [word]
  second <- known $ toList $ edits1 word
  third <- known_edits2 word
  candidates <- return $ first `union` second `union` third

  return $ maxWord candidates knownWords
  where
    maxWord :: Set String -> Map String Int -> String
    maxWord candidates wordCounts = 
      fst $ fold (max wordCounts) ("", 0) candidates

    max :: Map String Int -> String -> (String, Int) -> (String, Int)
    max wordCounts word m@(maxWord, maxCount) =
      if count > maxCount then (word, count) else m
      where count = fromMaybe 1 (Map.lookup word wordCounts)

-- def correct(word):
--     candidates = known([word]) or known(edits1(word)) or known_edits2(word) or [word]
--     return max(candidates, key=NWORDS.get)

main :: IO ()
main = do 
  args <- getArgs
  mapM_ (\word -> correct word >>= putStrLn) args

--  nwords >>= putStrLn . show
--  readFile dataFile >>= putStrLn . show . splitWords

-- Testing --

instance Arbitrary Char where
  arbitrary     = frequency [(4, choose ('\33', '\128')), (1, return ' ')]
  coarbitrary c = variant (ord c `rem` 4)

prop_words_nospaces s = all (not . elem ' ') (splitWords s)

prop_words_noempty s = all ((> 0) . length) (splitWords s)
