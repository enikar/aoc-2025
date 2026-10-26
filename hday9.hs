-- 2026-10-26, AoC day 9

-- {-# LANGUAGE ImportQualifiedPost #-}

module Main(main) where


import System.IO (readFile')
import Data.List (maximumBy)
import Data.Ord (comparing)

import Control.Monad (void)
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP
  (ReadP
  ,readP_to_S
  ,sepBy1
  ,optional
  ,munch1
  ,eof
  ,char
  )


type Coord = (Int, Int)


main :: IO ()
main = do
  coords <- getDatas "day9.txt"
  printSolution "Part1" (part1 coords)

-- ascending, descending :: Ord a => (b -> a) -> b -> b -> Ordering
-- ascending = comparing
-- descending = flip . comparing

area :: (Coord, Coord) -> Int
area ((x,y), (x',y')) = (abs (x - x') + 1) * (abs (y - y') + 1)
-- for part1, we try to split coordinates in 4 quaters.
-- - first with (x, y) < (xmean, ymean)
-- - second with (x, y) with x < xmean and y >= ymean
-- - third with (xmax, ymin) with x >= xmean and y < ymean
-- - fourth with (xmax, ymax) >= (xmean, ymean)
-- Then try to found the maximum between two opposite quarter
-- that is to say, the maximum area between first and the fourth,
-- and the maximum area between second and the third.
-- So we need to compute the x mean and the y mean, for all coords.
-- to build quarters.
-- It works, maybe it's just by chance, there is nothing to say our
-- supposition is true.
part1 :: [Coord] -> Int
part1 coords = max (area m1)  (area m2)
  where
    c0 = coords !! 0
    (xmin, xmax, ymin, ymax) = foldl' g (fst c0, fst c0, snd c0, snd c0) (drop 1 coords)
      where
        g (xd, xu, yd, yu) (x, y)
          | x < xd && y < yd = (x,xu, y, yu)
          | x < xd && y > yu = (x, xu, yd, y)
          | x > xu && y < yd = (xd, x, y, yu)
          | x > xu && y > yu = (xd, x, yd, y)
          | otherwise        = (xd, xu, yd, yu)

    xmean = (xmin + xmax) `quot` 2
    ymean = (ymin + ymax) `quot` 2

    (first, second, third, fourth) = foldl' f ([],[],[],[]) coords
      where
        f (first', second', third', fourth') p@(x, y)
          | x <= xmean && y <= ymean = (p:first', second', third', fourth')
          | x <= xmean && y > ymean  = (first', p:second', third', fourth')
          | x > xmean && y <= ymean  = (first', second', p:third', fourth')
          | otherwise                = (first', second', third', p:fourth')

    m1 = maximumBy (comparing area) (liftA2 (,) first fourth)
    m2 = maximumBy (comparing area) (liftA2 (,) second third)

-- the brute force solution
part1' :: [Coord] -> Int
part1' coords = area (maximumBy (comparing area) pairs)
  where
    pairs = liftA2 (,) coords coords

printSolution :: Show a => String -> a -> IO ()
printSolution part x = putStrLn (part <> ": " <> show x)

getDatas :: String -> IO [Coord]
getDatas filename = parseDatas <$> readFile' filename

parseDatas :: String -> [Coord]
parseDatas str = case readP_to_S parseCoords str of
                   [x] -> fst x
                   _   -> error "ParseDatas: can't parse."

positive :: ReadP Int
positive = read <$> munch1 isDigit

parseCoords :: ReadP [Coord]
parseCoords = do
  coords <- sepBy1 parseCoord (char '\n')
  optional (char '\n')
  eof
  pure coords

parseCoord :: ReadP Coord
parseCoord = do
  n1 <- positive
  void (char ',')
  n2 <- positive
  pure (n1,n2)
