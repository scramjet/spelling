{-# LANGUAGE BangPatterns #-}
module Main (main) where

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString, pack, unpack)
import qualified Data.ByteString as BS
import Data.Bits
import Data.Word (Word8)
import Data.Map (Map, findWithDefault, insertWith', empty, member)
import qualified Data.Map as Map (empty)
import Data.Set (toList, fromList)
import Data.List (inits, tails, foldl')
import System.Environment (getArgs)
import System.CPUTime (getCPUTime)
import Text.Printf

type WordFreq = Map ByteString Int

dataFile = "big.txt"
alphabet = "abcdefghijklmnopqrstuvwxyz"

splitWords :: ByteString -> [ByteString]
splitWords = 
  filter (not . BS.null) . BS.splitWith notLetter . BS.map mkLower
  where mkLower :: Word8 -> Word8
        mkLower x = x .|. 32

        notLetter :: Word8 -> Bool
        notLetter c = c < 97 || c > 122

train :: [ByteString] -> WordFreq
train = foldl' updateMap Map.empty
  where updateMap model word = insertWith' (+) word 1 model

nwords :: IO WordFreq
nwords = (return $!) . train . splitWords =<< B.readFile dataFile

edits1 :: String -> [String]
edits1 s = deletes ++ transposes ++ replaces ++ inserts
 where
   deletes = [a ++ bs | (a, _:bs) <- splits]
   transposes = [a ++ (b2:b1:bs) | (a, b1:b2:bs) <- splits]
   replaces = [a ++ (c:bs) | (a, _:bs) <- splits, c <- alphabet]
   inserts = [a ++ (c:b) | (a, b) <- splits, c <- alphabet]
   splits = zip (inits s) (tails s)

correct :: WordFreq -> String -> String
correct wordCounts word =
  unpack . fst $ foldl' maxCount (pack "?", 0) candidates
  where
    candidates :: [ByteString]
    candidates =
      known [word] `or` ((known e1) `or` known_edits2)

    e1 :: [String]
    e1 = toList . fromList $ edits1 word

    known_edits2 :: [ByteString]
    known_edits2 =
      [w3 | w1 <- e1, w2 <- edits1 w1, let w3 = pack w2, 
                                       w3 `member` wordCounts]

    known :: [String] -> [ByteString]
    known ws = [w | w <- map pack ws, w `member` wordCounts]

    maxCount :: (ByteString, Int) -> ByteString -> (ByteString, Int)
    maxCount current@(_, currentMax) word
      | count > currentMax = (word, count)
      | otherwise          = current
      where count = findWithDefault 1 word wordCounts

    or :: [ByteString] -> [ByteString] -> [ByteString]
    or a b | null a     = b
           | otherwise  = a

main :: IO ()
main = do 
  start <- getCPUTime
  args <- getArgs
  wordCounts <- nwords
  mapM_ (printCorrect wordCounts) args
  end <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)
  printf "Computation time: %0.3f sec\n" (diff :: Double)
  where
    printCorrect :: WordFreq -> String -> IO ()
    printCorrect wordCounts word =
      putStrLn $ word ++ " -> " ++ correct wordCounts word

 
-- time :: IO t -> IO t
-- time a = do
--     start <- getCPUTime
--     v <- a
--     end   <- getCPUTime
--     let diff = (fromIntegral (end - start)) / (10^12)
--     printf "Computation time: %0.3f sec\n" (diff :: Double)
--     return v

-- main :: IO () 
-- main = do
--     args <- getArgs        
--     wordCounts <- nwords
--     putStrLn "Starting..."
--     time $! (run wordCounts args) `seq` return ()
--     putStrLn "Done."
--     where run wordCounts args = do 
--           mapM_ (printCorrect wordCounts) args
--           where
--             printCorrect :: WordFreq -> String -> IO ()
--             printCorrect wordCounts word =
--                 putStrLn $ word ++ " -> " ++ correct wordCounts word

-- Testing --

-- instance Arbitrary Char where
--   arbitrary     = frequency [(4, choose ('\33', '\128')), (1, return ' ')]
--   coarbitrary c = variant (ord c `rem` 4)

-- prop_words_nospaces s = all (not . elem ' ') (splitWords s)

-- prop_words_noempty s = all ((> 0) . length) (splitWords s)
