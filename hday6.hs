-- 2026-10-07 AoC 2025 day 6

module Main where

import System.IO (readFile')
import Data.List (transpose)
import Data.List.Extra (split)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

data Pb = Pb String [Int] deriving (Show)

readInt :: String -> String -> Int
readInt funname n = fromMaybe errReadInt  (readMaybe n)
  where
    errReadInt = error (funname <>  ": expected an Int, got: " <> n)

parsePart1 :: String -> [Pb]
parsePart1 str = foldr f [] (transpose (map words (lines str)))
  where
    f line acc = foldr g (Pb "" []) line : acc
    g word (Pb op nums)
      | word == "+" || word == "*" = Pb word nums
      | otherwise   = Pb op (readInt "parsePart1" word : nums)


-- Read numbers on column wasn't natural.
-- We need to change the number lay out to treat them.
-- We noticed the result of "transpose (init (lines str))" was almost
-- what we wanted. Then we just split the result on field with only spaces
-- to get the columns with numbers as string.
-- Another way to solve that is to use a 2 dimensional Array of Char.
-- But that's another story.
parsePart2 :: String -> [Pb]
parsePart2 str = zipWith Pb ops nums
  where
    ls = lines str
    ls' = transpose (init ls)
    spaces = replicate (length ls - 1) ' '
    nums = map (map (readInt "parsePart2")) (split (==spaces) ls')
    ops = words (last ls)


printSolution :: Show a => String -> a -> IO ()
printSolution part x = putStrLn (part <> ": " <> show x)

main :: IO ()
main = do
  str <- readFile' "day6.txt"
  printSolution "Part1" (solve (parsePart1 str))
  printSolution "Part2" (solve (parsePart2 str))


solve :: [Pb] -> Int
solve pbs = foldl' f 0 pbs
  where
    f acc (Pb op nums)
      | op == "+" = acc + sum nums
      | otherwise = acc + product nums
