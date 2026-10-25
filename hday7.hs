-- 2026-10-21, AoC 2025 day 7
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}


module Main (main) where

import System.IO (readFile')
import Data.List
  (elemIndex
  ,elemIndices
  )
import Data.Maybe (fromMaybe)
import Data.IntMap.Strict qualified as Map
import Data.IntMap.Strict
  (IntMap
  ,intersection
  ,insertWith
  ,(\\)
  )

-- An (IntMap Int) reprensents the timelines at each step.
-- The Key is the column number of timelines.
-- The value is the number of timelines in this column.

-- A [IntMap Char] represents splitter coordinates at each step.
-- The Key is the column numbers of splitters.
-- We don't use the value associated with the splitter coordinates.
-- That can be anything. That simplifies the computing of part2.
type Board = (IntMap Int, [IntMap Char])

getBoard :: String -> IO Board
getBoard filename = buildBoard <$> readFile' filename
  where
    buildBoard str = (Map.singleton start 1, splitters)
      where
        ls = filter (any (/= '.')) (lines str)
        start = fromMaybe errorGetBoard (elemIndex 'S' (ls !! 0))
        errorGetBoard = error "getBoard: can't find the starting tachyon."
        splitters = map (Map.fromList . \x -> map (,'^') (elemIndices '^' x))
                        (drop 1 ls)

printSolution :: Show a => String -> a -> IO ()
printSolution part x = putStrLn (part <> ": " <> show x)

main :: IO ()
main = do
  board <- getBoard "day7.txt"
  let (p1, p2) = solve board
  printSolution "Part1" p1
  printSolution "Part2" p2


-- for part1, we just count the collisions with splitters
-- for part2, we build an IntMap of timelines, then sum all
-- timeline counts of the final IntMap.
solve :: Board -> (Int, Int)
solve (start, board) = finalize (foldl' next (0, start) board)
  where
    finalize (p1, timelines) = (p1, sum timelines)

    next (count, timelines) splitters = (count', timelines'')
      where
        collisions = intersection timelines splitters
        count' = count + length collisions
        timelines' = timelines \\ collisions

        timelines'' = Map.foldlWithKey' g timelines' collisions
          where
            g acc tachyon n = insertWith
                                (+)
                                (tachyon+1)
                                n
                                (insertWith (+) (tachyon-1) n acc)
