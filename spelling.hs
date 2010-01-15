import Data.Char (toLower, ord)
import Data.Map (Map, fromListWith, keysSet)
import Data.Set (Set, fromList, toList, member)
import Data.List (inits, tails)
import Data.List.Split (wordsBy)
import Test.QuickCheck

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

edits1 :: String -> [String]
edits1 s = set (deletes ++ transposes ++ replaces ++ inserts)
  where
    deletes    = [a ++ bs | (a, _:bs) <- splits]
    transposes = [a ++ (b2:b1:bs) | (a, b1:b2:bs) <- splits]
    replaces   = [a ++ (c:bs) | (a, _:bs) <- splits, c <- alphabet]
    inserts    = [a ++ (c:b) | (a, b) <- splits, c <- alphabet]
    splits     = zip (inits s) (tails s)

-- def known_edits2(word):
--     return set(e2 for e1 in edits1(word) for e2 in edits1(e1) if e2 in NWORDS)

known_edits2 :: String -> IO [String]
known_edits2 s = do
  knownWords <- nwords >>= return . keysSet
  return $ set [e2 | e1 <- edits1 s, e2 <- edits1 e1, e2 `member` knownWords]

--     splits :: String -> [(String, String)]
--     splits = splits' ""
--       where
--         splits' a b@(hd:tl) = (a, b) : splits' (a ++ [hd]) tl
--         splits' h "" = [(h, "")]

main :: IO ()
main = do 
  nwords >>= putStrLn . show
--    readFile dataFile >>= putStrLn . show . splitWords

-- Testing --

instance Arbitrary Char where
  arbitrary     = frequency [(4, choose ('\33', '\128')), (1, return ' ')]
  coarbitrary c = variant (ord c `rem` 4)

prop_words_nospaces s = all (not . elem ' ') (splitWords s)

prop_words_noempty s = all ((> 0) . length) (splitWords s)
