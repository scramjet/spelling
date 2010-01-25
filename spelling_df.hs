{-# LANGUAGE BangPatterns #-}
module Main (main) where

import qualified Data.ByteString.Char8 as B
import Data.Char (toLower)
import Data.Map (Map, findWithDefault, insertWith', member)
import qualified Data.Map as Map (empty)
import Data.Set as Set (Set, fromList, toList, fold, null)
import Data.List (inits, tails, foldl')
import System.Environment (getArgs)

dataFile = "big.txt"
alphabet = "abcdefghijklmnopqrstuvwxyz"

splitWords :: B.ByteString -> [B.ByteString]
splitWords = {-# SCC "filter" #-} filter (not . B.null) . {-# SCC "splitWith" #-} B.splitWith isNogud . {-# SCC "toLower" #-} B.map toLower

isNogud :: Char -> Bool
isNogud c = c < 'a' || 'z' < c

train :: [B.ByteString] -> Map B.ByteString Int
train = foldl' updateMap Map.empty
 where 
   updateMap model word = insertWith' (+) word 1 model

myReadFile = B.readFile dataFile

nwords :: IO (Map B.ByteString Int)
nwords = return . train . splitWords =<< myReadFile

edits1 :: String -> [String]
edits1 s = toList . fromList $ deletes ++ transposes ++ replaces ++ inserts
 where
   deletes = [a ++ bs | (a, _:bs) <- splits]
   transposes = [a ++ (b2:b1:bs) | (a, b1:b2:bs) <- splits]
   replaces = [a ++ (c:bs) | (a, _:bs) <- splits, c <- alphabet]
   inserts = [a ++ (c:b) | (a, b) <- splits, c <- alphabet]
   splits = zip (inits s) (tails s)

correct :: Map B.ByteString Int -> String -> String
correct wordCounts word = B.unpack . fst $ fold maxCount (B.pack "?", 0)  candidates
 where
   candidates :: Set B.ByteString
   candidates =
     known [word] `or` ((known $ edits1 word) `or` known_edits2 word)

   known_edits2 :: String -> Set B.ByteString
   known_edits2 w =
     fromList [w3 | w1 <- edits1 w, w2 <- edits1 w1
                   , let w3 = B.pack w2, w3 `member` wordCounts]

   known :: [String] -> Set B.ByteString
   known ws = fromList [w | w <- map B.pack ws, w `member` wordCounts]

   maxCount :: B.ByteString -> (B.ByteString, Int) -> (B.ByteString, Int)
   maxCount word current@(_, currentMax)
     | count > currentMax = (word, count)
     | otherwise = current
       where 
         count = findWithDefault 1 word wordCounts

   or :: Set B.ByteString -> Set B.ByteString -> Set B.ByteString
   or a b | Set.null a = b
          | otherwise = a

main :: IO ()
main = do
 args <- getArgs
 wordCounts <- nwords
 mapM_ (printCorrect wordCounts) args
 where
   printCorrect :: Map B.ByteString Int -> String -> IO ()
   printCorrect wordCounts word =
     putStrLn $ word ++ " -> " ++ correct wordCounts word
