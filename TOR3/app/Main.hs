module Main (main) where

import Data.ByteString.Lazy (pack, readFile, unpack, writeFile)
import Data.Char (chr, digitToInt, ord)
import Data.List (foldl', sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Ord (Down (Down))
import Data.Word (Word8)
import GHC.Float (int2Double)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["file", x] -> do
      content <- unpack <$> Data.ByteString.Lazy.readFile x
      Data.ByteString.Lazy.writeFile "out.bin" (pack $ bitString2Bytes $ shannonFano content)
    ["string", x] -> do
      let bitString = shannonFano $ map (fromIntegral . ord) x
      putStrLn bitString
      putStrLn $ map (chr . fromIntegral) $ bitString2Bytes bitString
    _ -> runTests

runTests :: IO ()
runTests = putStrLn "running"

shannonFano :: [Word8] -> String
shannonFano s = do
  let occ = occurances s
  let freq = map (\x -> (fst x, int2Double (snd x) / int2Double (length s))) occ
  let coded = concatMap (fanoCode freq) s
  coded
 where
  occurances :: [Word8] -> [(Word8, Int)]
  occurances = M.toList . foldl' (\acc x -> M.insertWith (+) x 1 acc) M.empty

  fanoCode :: [(Word8, Double)] -> Word8 -> String
  fanoCode freq word = fromJust $ lookup word $ genFanoCode "" (sortOn (Down . snd) freq)

  genFanoCode :: String -> [(Word8, Double)] -> [(Word8, String)]
  genFanoCode _ [] = []
  genFanoCode pref [(word, _)] = [(word, reverse pref)]
  genFanoCode pref freq = genFanoCode ('1' : pref) mostFreq ++ genFanoCode ('0' : pref) leastFreq
   where
    (mostFreq, leastFreq) = splitFreqInHalf freq

  splitFreqInHalf :: [(Word8, Double)] -> ([(Word8, Double)], [(Word8, Double)])
  splitFreqInHalf [] = ([], [])
  splitFreqInHalf [a] = ([a], [])
  splitFreqInHalf freq = split freq []
   where
    total = sum (map snd freq)

    split :: [(Word8, Double)] -> [(Word8, Double)] -> ([(Word8, Double)], [(Word8, Double)])
    split [lastItem] acc = (reverse acc, [lastItem])
    split (x : xs) [] = split xs [x]
    split [] acc = (reverse acc, [])
    split (x : xs) acc =
      let currentDiff = abs (total - 2 * sum (map snd acc))
          nextDiff = abs (total - 2 * sum (map snd (x : acc)))
       in if nextDiff <= currentDiff
            then split xs (x : acc)
            else (reverse acc, x : xs)

bitString2Bytes :: String -> [Word8]
bitString2Bytes = map binToWord8 . chunksOf8

chunksOf8 :: String -> [String]
chunksOf8 [] = []
chunksOf8 s = let (first, rest) = splitAt 8 s in padRight 8 '0' first : chunksOf8 rest

padRight :: Int -> Char -> String -> String
padRight n c s = s ++ replicate (n - length s) c

binToWord8 :: String -> Word8
binToWord8 = foldl' (\acc x -> acc * 2 + fromIntegral (digitToInt x)) 0
