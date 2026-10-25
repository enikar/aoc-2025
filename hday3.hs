-- 2026-10-03, AoC 2025 day 3
-- 2026-10-04: refactor the code
module Main where

import System.IO (readFile')
import Data.Char (digitToInt)

getDatas :: String -> IO [[Int]]
getDatas filename = parseDatas <$> readFile' filename
  where
    parseDatas str = map digitToInt <$> lines str


printSolution :: Show a => String -> a -> IO ()
printSolution part x = putStrLn (part <> ": " <> show x)

main :: IO ()
main = do
  datas <- getDatas "day3.txt"
  printSolution "Part1" (solve 2 datas)
  printSolution "Part2" (solve 12 datas)

-- solve calls buildGreaterNumber for each batteries list in datas.
-- d is the number of digits of searched numbers.
-- In buildGreaterNumber we loop over remains which is a list
-- of number we need to leave at the end, to ensure to build
-- a number with d digits.
solve :: Int -> [[Int]] -> Int
solve d datas = foldl' (buildGreaterNumber remains) 0 datas
  where
    remains = [d-1, d-2..0]

-- buildGreaterNumber may be "simplified", passing the new list of
-- digits inside the accumulator. Thus we won't need to calculate the
-- new position in bats. But, then we'll need to recompute the list
-- length at each iteration.
-- Maybe it's possible to build the number in
-- one pass over the list bats. Here, we walk through parts of bats many
-- times (in maximumWithIndex).

-- buildGreaterNumber loop over remains which is the number of digits
-- we leave at the end of bats for each iteration.
buildGreaterNumber :: [Int] -> Int -> [Int] -> Int
buildGreaterNumber remains n bats = n + convert digits
      where
        lg = length bats
        (digits, _)  = foldl' g ([], 0) remains
          where
            -- len is the number of elments we drop at the begining of
            -- bats.
            -- acc is the list of collected digits.
            -- remainder is the number of digits we don't take in count
            -- at the end of bats.
            -- idx is the index of the new maximum.
            -- to recompute the length we need to drop at the next iteration,
            -- we sum the old length len, and idx+1 (indices begin at 0).
            g (acc, len) remainder = (digit:acc, len+idx+1)
              where
                ls = drop len (take (lg-remainder) bats)
                (digit, idx) = maximumWithIndex ls


maximumWithIndex :: [Int] -> (Int, Int)
-- Using maximumBy doesn't work because it doesn't find the first index
-- but the last. (I suspect the use of foldr).
-- maximumWithIndex ls = maximumBy (compare `on` fst) (zip ls [0..])
maximumWithIndex ls = foldl' g (0, 0) (zip ls [0..])
  where
    g (m, mindex) (v, index)
      | v > m     = (v, index)
      | otherwise = (m, mindex)

-- convert calculates the number from the collected digits.
-- digits are in the reversed order: first digits are the meaningless.
-- That's why we use foldr. It avoids the need to reverse the list of digits.
convert :: [Int] -> Int
convert digits = foldr f 0 digits
  where
    f digit acc = acc * 10 + fromIntegral digit
