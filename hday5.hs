-- 2026-10-05, AoC 2025 day 5
-- Finished on 2026-10-07
module Main where

import System.IO
  (readFile'
  ,withFile
  ,IOMode(..)
  ,hPutStrLn
  )
import System.PosixCompat (fileExist)
import Data.Char (isDigit)
import Control.Monad (void)
import Data.Ix (inRange)
import Data.List (sortOn)
import Data.Foldable (for_)
import Text.ParserCombinators.ReadP
  (ReadP
  ,readP_to_S
  ,char
  ,string
  ,sepBy1
  ,munch1
  ,optional
  ,eof
  )


type Range = (Int, Int)

printSolution :: Show a => String -> a -> IO ()
printSolution part x = putStrLn (part <> ": " <> show x)

main :: IO ()
main = do
  (ranges, ids) <- getDatas "day5.txt"
  printSolution "Part1" (part1 ranges ids)
  printSolution "Part2" (part2 ranges)


part1 :: [Range] -> [Int] -> Int
part1 ranges ids = foldl' f 0 ids
  where
    f acc i
      | isFresh i ranges = acc + 1
      | otherwise        = acc

isFresh :: Int -> [Range] -> Bool
isFresh i ranges = foldr f False ranges
  where
    f range acc
      | acc       = True
      | otherwise = inRange range i

-- part2 is not straightforward because ranges can overlap.
-- But sort the ranges on fst is sufficient.
-- The trick is to initialize the foldl' with a value that count to zero
-- and finally add the last range, once we left the foldl'.
part2 :: [Range] -> Int
part2 ranges = count + countRange theLast
  where
    sorted = sortOn fst ranges
    countRange rg = snd rg - fst rg + 1
    (count, theLast) = foldl' f (0,(0,-1)) sorted
    f (cnt, waiting) (begin, end)
      | snd waiting >= end   = (cnt, waiting)
      | snd waiting >= begin = (cnt, (fst waiting, end))
      | otherwise            = (cnt + countRange waiting, (begin, end))


-- Parsing stuff.
getDatas :: String -> IO ([Range], [Int])
getDatas filename = parseDatas <$> readFile' filename

parseDatas :: String -> ([Range], [Int])
parseDatas str = case readP_to_S readDatas str of
                   [x] -> fst x
                   _   -> error "parseDatas: Cannot parse."

readDatas :: ReadP ([Range], [Int])
readDatas = do
  rgs <- sepBy1 readRange (char '\n')
  void (string "\n\n")
  nums <- sepBy1 positive (char '\n')
  optional (char '\n')
  eof
  pure (rgs, nums)

positive :: ReadP Int
positive = read <$> munch1 isDigit

readRange :: ReadP Range
readRange =
  liftA2 (,)
         positive
         (char '-' *> positive)

-- alternative version for readRange
readRange' :: ReadP Range
readRange' =
  (,)
  <$> positive
  <*> (char '-' *> positive)

-- Definitions to see how the datas are shaped, to use in ghci.
-- testRanges takes a filename and the list of range as arguments.
-- The filename is written if it doesn't exist or if we allow to overwrite
-- it.
testRanges :: String -> [Range] -> IO ()
testRanges filename ranges = do
    yes <- overwriteFile filename
    if not yes
    then putStrLn ("Gave up! We didn't overwrite:" <> " " <> filename)
    else do
         writeRanges filename ranges
         putStrLn (filename <> " is saved. We finished.")

-- overwrite takes a filename as argument.
-- Return True is the file doesn't exist or we allow to overwrite it.
overwriteFile :: String -> IO Bool
overwriteFile filename = do
  exist <- fileExist filename
  if exist
  then do
       putStrLn ("file: " <> filename <> " exists! Overwrite it? (y/n)")
       c <- getChar
       if c == 'y'
       then pure True
       else pure False
  else pure True

-- writeRanges write the numbered ranges sorted by fst and snd to
-- filename. There is no question.
writeRanges :: String -> [Range] -> IO ()
writeRanges filename ranges =
  let labeled = zip [(0::Int)..] ranges
      rgs = sortOn (fst . snd) labeled
      rgs' = sortOn (snd . snd) labeled
  in withFile
     filename
     WriteMode
     (\handle ->
         for_ (zip rgs rgs')
              (\(f, s) ->
                 let star = if fst f /= fst s then "*" else ""
                 in hPutStrLn handle (show f <> " " <> show s <> star)))
