-- 2026-10-04, AoC 2025, day 4

module Main where

import System.IO (readFile')
import Data.Array.Unboxed
  (UArray
  ,array
  ,assocs
  ,bounds
  ,(!)
  ,(//)
  )

import Data.Ix (inRange)

type Position = (Int,Int)
type Grid = UArray Position Char

getDatas :: String -> IO Grid
getDatas filename = buildGrid <$> readFile' filename
  where
    buildGrid str = array ((0,0), (width, heigth)) cs
      where
        ss = lines str
        heigth = length ss - 1
        width = case ss of
          []     -> 0
          (s:_) -> length s - 1
        cs = [((x,y), c)
             |(y,s) <- zip [0..] ss
             ,(x,c) <- zip [0..] s
             ]

printSolution :: Show a => String -> a -> IO ()
printSolution part x = putStrLn (part <> ": " <> show x)

main :: IO ()
main = do
  grid <- getDatas "day4.txt"
  printSolution "Part1" (part1 grid)
  printSolution "Part2" (part2' grid)

part1 :: Grid -> Int
part1 =  length . solve

-- for part2, we use until. Maybe, we should rewrite it using a stream
-- compute by iterate to be more haskellish.
part2 :: Grid -> Int
part2 grid0 = first (until satisfy improve (0, True, grid0))
  where
    first (x, _ , _) = x
    satisfy (_, change, _) = not change

    improve (count, _, grid) = (count', change, grid')
      where
        positions = solve grid
        change = not (null positions)
        count' = count + length positions
        grid' = grid // map (,'.') positions

-- Finally, using a stream is clearer.
part2' :: Grid -> Int
part2' grid0 = sum               -- we sum all collected count
               . takeWhile (/=0) -- we stop when no more roll paper can be removed
               . map fst         -- just keep the count, the grid is useless here
               . drop 1          -- the first element is meaningless
               . iterate update $ (0, grid0)
  where
    update (_, grid) = (count, grid')
      where
        positions = solve grid
        count = length positions
        grid' = grid // [(p, '.') | p <- positions]


-- solve returns the list of positions where it's possible
-- to remove a roll paper.
solve :: Grid -> [Position]
solve grid  = foldr f [] (assocs grid)
  where
    f (pos, c) acc
      | canBeRemoved pos c grid = pos:acc
      | otherwise               = acc

steps :: [(Int,Int)]
steps = [(x,y)
        |x <- [-1..1]
        ,y <- [-1..1]
        ,(x,y) /= (0,0)
        ]

canBeRemoved :: Position -> Char -> Grid -> Bool
canBeRemoved (xp, yp) c grid
  | c /= '@'  = False
  | otherwise = fst (foldr g (True, 0::Int) steps)
    where
      limits = bounds grid

      g (dx,dy) (notExit, count)
        | not notExit      = (False, 4)
        | otherwise = (count' < 4, count')
          where
            p = (xp+dx, yp+dy)
            count'
              | not (inRange limits p) = count
              | grid ! p == '@'        = count+1
              | otherwise              = count
