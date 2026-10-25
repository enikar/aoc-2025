-- 2026-10-01
-- AoC 2025 day 2

module Main where

import System.IO (readFile')
import Data.List.Extra (splitOn)
import Data.List (inits)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

data Range = Range Int Int deriving (Show)

getDatas :: String -> IO [Range]
getDatas filename = parseDatas <$> readFile' filename
  where
    parseDatas str = map mkRange ls
      where
        ls = splitOn "," str
        mkRange r = Range begin end
          where
            rs = splitOn "-" r
            begin = readInt (rs !! 0)
            end = readInt (rs !! 1)

    readInt s = fromMaybe errorReadInt (readMaybe s)
      where
        errorReadInt = error ("getDatas: readInt: "
                              <> s
                              <> " is not a valid Int.")

printSolution :: Show a => String -> a -> IO ()
printSolution part n = putStrLn (part <> ": " <> show n)

main :: IO ()
main = do
  ranges <- getDatas "day2.txt"
  printSolution "Part1" (partx malformed1 ranges)
  printSolution "Part2" (partx malformed2 ranges)


partx :: (String -> Bool) -> [Range] -> Integer
partx malformed ranges = foldl' f 0 ranges
  where
    f acc rg = acc + sum (map read ls)
      where
        rgs = buildRange rg
        ls = filter malformed rgs

malformed1 :: String -> Bool
malformed1 ns
    | odd lg    = False
    | otherwise = left == right
    where
      lg = length ns
      half = lg `quot` 2
      (left, right) = splitAt half ns

-- use Data.List.inits on the half list of ns, drop the the first element
-- which is [], and loop over others.
-- For each  one try to replicate the string to obtain the length of ns
-- and compare the result with ns.
-- maybe loop with foldr which permits to short-circuit.
-- The first writting was the good one!
malformed2 :: String -> Bool
malformed2 ns = foldr f False (drop 1 (inits half))
  where
    lg = length ns
    half = take (lg `div` 2) ns

    f _   True  = True
    f ns' False =  built == ns
      where
        n = lg `div` length ns'
        built = concat (replicate n ns')


buildRange :: Range -> [String]
buildRange (Range begin end) = map show [begin..end]
