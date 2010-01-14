import Data.Char (toLower, ord)
import Data.Map (Map, fromListWith)
import Data.Set (Set, fromList)
import Data.List (inits, tails)
import Data.List.Split (wordsBy)
import Text.Regex (mkRegexWithOpts, splitRegex)
import Test.QuickCheck

dataFile = "medium.txt"
alphabet = "abcdefghijklmnopqrstuvwxyz"

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
