import Data.Char (toLower)
import Data.Map (Map, fromListWith)
import Text.Regex (mkRegexWithOpts, splitRegex)

words' :: String -> [String]
words' = splitRegex boundary . map toLower
         where boundary = mkRegexWithOpts "[^a-z]+" False True

train :: [String] -> Map String Int
train = fromListWith (+) . map (\i -> (i, 1))

nwords :: IO (Map String Int)
nwords = readFile "small.txt" >>= return . train . words'

test :: IO ()
test = do
  text <- readFile "small.txt" 
  putStrLn $ show $ words' text
