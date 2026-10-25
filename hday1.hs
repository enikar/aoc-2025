-- Aoc 2025, day1

{- HLINT ignore "Eta reduce" -}

module Main where

import Data.Char (isDigit)
import System.IO (readFile')
import Text.ParserCombinators.ReadP
  (ReadP
  ,readP_to_S
  ,sepBy1
  ,char
  ,optional
  ,eof
  ,(<++)
  ,munch1
  )

showSolution :: String -> Int -> IO ()
showSolution s n = putStrLn (s <> ": " <> show n)

main :: IO ()
main = do
  datas <- parseDatas <$> readFile' "day1.txt"
  showSolution "Part1" (part1 datas)
  showSolution "Part2" (part2 datas)

-- part1: counts the times we stopped at 0
part1 :: [Int] -> Int
part1 steps = fst  (foldl' f (0,50) steps)
  where
    f (cnt, pos) clicks =
      let pos' = (pos+clicks) `mod` 100
      in if pos' == 0
         then (cnt+1, pos')
         else (cnt, pos')

-- part2 counts the times we passed over 0
part2 :: [Int] -> Int
part2 steps = fst (foldl' f (0,50) steps)
  where
    f (cnt, pos) clicks = (cnt+cnt', pos'')
      where
        pos' = pos + clicks
        pos'' = pos' `mod` 100
        ds | pos' > pos = [pos+1 .. pos']
           | otherwise  = [pos' .. pos-1]
        cnt' = length (filter ((==0) . (`mod` 100)) ds)

part2' :: [Int] -> Int
part2' steps = fst (foldl' f (0,50) steps)
  where
    f (cnt, pos) clicks = (cnt+cnt', pos')
      where
        npos = pos + clicks
        (qpos, pos') = npos `divMod` 100
        cnt' | clicks > 0            = qpos
             | pos == 0              = abs qpos - 1
             | (npos `mod` 100) == 0 = abs qpos + 1
             | otherwise             = abs qpos

-- Parsing.
parseDatas :: String -> [Int]
parseDatas str = case readP_to_S readDatas str of
  [x] -> fst x
  _   -> error "Error: parseDatas: can't parse."

readDatas :: ReadP [Int]
-- readDatas = do
--   steps <- sepBy1 readStep (char '\n')
--   optional (char '\n')
--   eof
--   pure steps
readDatas = sepBy1 readStep (char '\n')
  <* (optional (char '\n') *> eof)

readStep :: ReadP Int
readStep = do
  dir <- readDir
  n   <- decimal
  pure (dir * n)

readDir :: ReadP Int
readDir = do
  dir <- char 'L' <++ char 'R'
  case dir of
    'L' -> pure (-1)
    'R' -> pure 1
    _   -> error ("readDir: " <> [dir] <> " is not a L or a R")

decimal :: ReadP Int
decimal = read <$> munch1 isDigit
