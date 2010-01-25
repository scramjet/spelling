{-# LANGUAGE BangPatterns #-}
module Main (main) where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as BS
import Data.Bits
import Data.Word (Word8)
import Data.Char (toLower)
import Data.Map (Map, findWithDefault, insertWith', keysSet, empty, member)
import qualified Data.Map as Map (lookup, empty, size)
import Data.Set (toList, fromList)
import Data.List (inits, tails, foldl')
import System.Environment (getArgs)

dataFile = "big.txt"
alphabet = "abcdefghijklmnopqrstuvwxyz"

splitWords :: B.ByteString -> [B.ByteString]
splitWords = {-# SCC "filter" #-} filter (not . BS.null) . {-# SCC "splitWith" #-} BS.splitWith isNogud . {-# SCC "mkLow" #-} BS.map mkLow

mkLow :: Word8 -> Word8
mkLow x = x .|. 32

isNogud :: Word8 -> Bool
isNogud c = c < 97 || 122 < c

train :: [B.ByteString] -> Map B.ByteString Int
train = foldl' updateMap Map.empty
 where updateMap model word = {-# SCC "updateMap" #-} insertWith' (+) word 1 model

myReadFile = B.readFile dataFile

nwords :: IO (Map B.ByteString Int)
nwords = (return $!) . train . splitWords =<< myReadFile

edits1 :: String -> [String]
edits1 s = deletes ++ transposes ++ replaces ++ inserts
 where
   deletes = [a ++ bs | (a, _:bs) <- splits]
   transposes = [a ++ (b2:b1:bs) | (a, b1:b2:bs) <- splits]
   replaces = [a ++ (c:bs) | (a, _:bs) <- splits, c <- alphabet]
   inserts = [a ++ (c:b) | (a, b) <- splits, c <- alphabet]
   splits = zip (inits s) (tails s)

correct :: Map B.ByteString Int -> String -> String
correct wordCounts word = B.unpack $ maxCount (B.pack "?") 0 candidates
 where
   candidates :: [B.ByteString]
   candidates =
     known [word] `or` ((known e1) `or` known_edits2)

   e1 :: [String]
   e1 = toList . fromList $ edits1 word

   known_edits2 :: [B.ByteString]
   known_edits2 =
     [w3 | w1 <- e1, w2 <- edits1 w1, let w3 = B.pack w2, w3 `member` wordCounts]

   known :: [String] -> [B.ByteString]
   known ws = {-# SCC "known" #-} [w | w <- map B.pack ws, w `member` wordCounts]

   maxCount :: B.ByteString -> Int -> [B.ByteString] -> B.ByteString
   maxCount best cmax (word:more)
       | cmax < count  = maxCount word count more
       | otherwise     = maxCount best cmax more
         where
           count = findWithDefault 1 word wordCounts
   maxCount best _ _ = best

   or :: [B.ByteString] -> [B.ByteString] -> [B.ByteString]
   or a b | null a     = b
          | otherwise  = a

main :: IO ()
main = do
 args <- getArgs
 wordCounts <- nwords
 mapM_ (printCorrect wordCounts) args
 where
   printCorrect :: Map B.ByteString Int -> String -> IO ()
   printCorrect wordCounts word =
     putStrLn $ word ++ " -> " ++ correct wordCounts word
