import Data.Char (toLower, ord)
import Data.Map (Map, fromListWith)
import Data.Set (Set, fromList)
import Data.List (inits, tails)
import Text.Regex (mkRegexWithOpts, splitRegex)
import Test.QuickCheck
 
dataFile = "small.txt"

words' :: String -> [String]
words' = splitRegex boundary . map toLower
  where boundary = mkRegexWithOpts "[^a-z]+" False True

train :: [String] -> Map String Int
train = fromListWith (+) . map (\s -> (s, 1))

nwords :: IO (Map String Int)
nwords = readFile dataFile >>= return . train . words'

edits1 :: String -> Set String
edits1 s = fromList (deletes ++ transposes ++ replaces ++ inserts)
  where
    deletes    = [a ++ bs | (a, _:bs) <- splits s]
    transposes = undefined
    replaces   = undefined
    inserts    = undefined
    splits s   = zip (inits s) (tails s)

--     splits :: String -> [(String, String)]
--     splits = splits' ""
--       where
--         splits' a b@(hd:tl) = (a, b) : splits' (a ++ [hd]) tl
--         splits' h "" = [(h, "")]
    
test :: IO ()
test = do 
  text <- readFile "small.txt" 
  putStrLn $ show $ words' text

-- Testing --

instance Arbitrary Char where
  arbitrary     = frequency [(4, choose ('\33', '\128')), (1, return ' ')]
  coarbitrary c = variant (ord c `rem` 4)

prop_words_nospaces s = all (not . elem ' ') (words' s)

prop_words_noempty s = all ((> 0) . length) (words' s)
