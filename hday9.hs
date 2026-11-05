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
type Rectangle = (Coord, Coord)

main :: IO ()
main = do
  coords <- getDatas "day9.txt"
  printSolution "Part1" (part1 coords)
  -- printSolution "Part2" (part2 coords)

-- Compute the area of a rectangle shaped by its opposite corners.
area :: Rectangle -> Int
area ((x,y), (x',y')) = (abs (x - x') + 1) * (abs (y - y') + 1)

-- First we compute the corners of rectangle where all the coords lie.
-- Then, we search for each corner what the coord is closer.
-- Finally, we work out the area of the largest rectangle.
part1 :: [Coord] -> Int
part1 coords = max (area (m1,m4)) (area (m2,m3))
  where
    c0 = coords !! 0
    (c1,c2,c3,c4) = ((xmin,ymin), (xmin,ymax), (xmax,ymin), (xmax,ymax))

    (xmin,xmax,ymin,ymax) = limits coords

    distance ((x,y),(x',y')) = dx*dx + dy*dy
      where
        dx = x - x'
        dy = y - y'

    mind c x y
      | distance(x,c) < distance(y,c) = x
      | otherwise                     = y

    -- m1 is the tile closer from c1
    -- m2 is the tile closer from c2
    -- and so on…
    (m1, m2, m3, m4) = foldl' g (c0,c0,c0,c0) (drop 1 coords)
      where g (m1', m2', m3', m4') c = (mind c1 m1' c
                                       ,mind c2 m2' c
                                       ,mind c3 m3' c
                                       ,mind c4 m4' c
                                       )

limits :: [Coord] -> (Int, Int, Int, Int)
limits coords = foldl' f (fst c0, fst c0, snd c0, snd c0) (drop 1 coords)
  where
    c0 = coords !! 0
    f (xmin, xmax, ymin, ymax) (x, y) = (min xmin x
                                        ,max xmax x
                                        ,min ymin y
                                        ,max ymax y
                                        )

-- a brute force solution, but slower than part1
part1' :: [Coord] -> Int
part1' coords = area (maximumBy (comparing area) (pairs coords))
  where
    pairs []     = []
    pairs [_]    = []
    pairs (x:xs) = map (x,) xs ++ pairs xs

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
