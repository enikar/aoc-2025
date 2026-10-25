-- 2026-10-25, Aoc 2025 day8
{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import System.IO (readFile')
import Data.Vector qualified as V
import Data.Vector (Vector, (!))
import Data.Set qualified as Set
import Data.Set
  (Set
  --,size is not needed, Set are Foldable, so we use length
  ,member
  ,union
  ,(\\)
  )
import Data.List (sortOn, sortBy)
import Data.Functor ((<&>))
import Control.Monad.Extra (loop)
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

type Coord = (Int, Int, Int)

toPair :: [Coord] -> (Coord, Coord)
toPair [c1, c2] = (c1, c2)
toPair _        = error "toPair expects a two element list."

main :: IO ()
main = do
 coords <- parseDatas <$> readFile' "day8.txt"
 let combs = map toPair (combinationsOpt 2 (V.fromList coords))
     combs' = sortOn (uncurry distance) combs
 printSolution "Part1" (part1 1000 combs')
 printSolution "Part2" (part2 combs')

descending :: Ord a => a -> a -> Ordering
descending = flip compare

-- well, it's not very nice, but it works.
-- Finally, I use loop because it's cleaner than foldr in this case.
-- There still is a mystery. I increment the count each time I connect a
-- pair of boxes. Then I thought taking m combs before connecting them
-- should work… but it didn't!
part1 :: Int -> [(Coord, Coord)] -> Int
part1 m combs = product (take 3 junctSize)
  where
    -- Note: we use Set.toAscList, toList is just an alias for it. We
    -- don't use (Set.map length junctions) because if two sets have the
    -- same size the resulting set will have only one. We want to keep
    -- the size of all sets. Else we could have used:
    -- Set.toDescList (Set.map length junctions)
    junctSize = sortBy descending (map length (Set.toAscList junctions))

    junctions = loop next (0, Set.empty, combs)

    next (_, _, []) = error "part1: no solutions."
    next (count, sets, pair:pairs)
      | count == m  = Right sets
      | otherwise   = Left (count+1, connect pair sets, pairs)

-- First, we initialize the algorithm to have at least two set of connected
-- boxes, so they are in a set size > 1.
-- Second, we waits the set size of connected boxes sets becomes equals
-- to 1.
-- I wasted my time because the solution for the sample on AoC site is
-- wrong but part2 yields the right answer with my datas.
part2 :: [(Coord, Coord)] -> Int
part2 combs = x * x'
  where
    (sets0, combs') = loop initialize (Set.empty, combs)

    initialize (_, [])  = error "part2: initialization fails."
    initialize (sets, pair:pairs)
      | length sets > 1 = Right (sets, pairs)
      | otherwise       = Left (connect pair sets, pairs)

    ((x, _, _),(x', _, _)) = loop solve (sets0, combs')

    solve (_, [])         = error "part2: no solution."
    solve (sets, pair:pairs)
      | length sets' == 1 = Right pair
      | otherwise         = Left (sets', pairs)
        where
          sets' = connect pair sets

connect :: (Coord, Coord) -> Set (Set Coord) -> Set (Set Coord)
connect (c1, c2) sets =
  case Set.toAscList (Set.filter search sets) of
    []       -> Set.insert (Set.fromList [c1, c2]) sets
    [s]      -> update s
    [s1, s2] -> update' s1 s2
    _        -> error "connect with more than two sets!"
  where
    search s = c1 `member` s || c2 `member` s

    update s =
      let sets' = Set.delete s sets
          s' | c1 `member` s = Set.insert c2 s
             | otherwise     = Set.insert c1 s
      in Set.insert s' sets'

    update' s1 s2 =
      let sets' = Set.delete s1 (Set.delete s2 sets)
      in Set.insert (s1 `union` s2) sets'

-- A rewrite of connect which doesn't use Set.toAscList.
-- I'm not convinced it's better.
connect' :: (Coord, Coord) -> Set (Set Coord) -> Set (Set Coord)
connect' (c1, c2) sets
  | rSize == 0 = Set.insert (Set.fromList [c1, c2]) sets
  | rSize == 1 = update
  | otherwise  = update'
    where
      search s = c1 `member` s || c2 `member` s
      rSearch = Set.filter search sets
      rSize = length rSearch

      update =
        let s = Set.elemAt 0 rSearch
            sets' = Set.delete s sets
            s' | c1 `member` s = Set.insert c2 s
               | otherwise     = Set.insert c1 s
        in Set.insert s' sets'

      -- Note: foldl' is applied to a set, so it is the same
      -- as Set.foldl' since Set are Foldable.
      update' = let sets' = sets \\ rSearch
                in Set.insert (foldl' union Set.empty rSearch) sets'


-- We don't use sqrt, because it's useless. sqrt is a strictly
-- increasing function, then that doesn't change the comparison order.
distance :: Coord -> Coord -> Int
distance (x, y, z) (x', y', z') = dx*dx+dy*dy+dz*dz
  where
    dx = x - x'
    dy = y - y'
    dz = z - z'

printSolution :: Show a => String -> a -> IO ()
printSolution part x = putStrLn (part <> ": " <> show x)

parseDatas :: String -> [Coord]
parseDatas str = case readP_to_S parseTriplets str of
                   [x] -> fst x
                   _   -> error "ParseDatas: can't parse."

positive :: ReadP Int
positive = read <$> munch1 isDigit

parseTriplets :: ReadP [Coord]
parseTriplets = do
  coords <- sepBy1 parseTriplet (char '\n')
  optional (char '\n')
  eof
  pure coords

parseTriplet :: ReadP Coord
parseTriplet = do
  n1 <- positive
  void (char ',')
  n2 <- positive
  void (char ',')
  n3 <- positive
  pure (n1, n2, n3)

combinationsOpt :: Int -> Vector Coord -> [[Coord]]
combinationsOpt k vec
  | k < 0      = error "combinationsOpt: bad arguments"
  | otherwise  = go 0 1 []
    where
      n = V.length vec - 1
      go start depth comb
        | depth == k = [start..n] <&> consComb
        | otherwise  = [start..n] >>= \i -> go (i+1) (depth+1) (consComb i)
               where
                 consComb i = vec ! i : comb
