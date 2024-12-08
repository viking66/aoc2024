#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (p: with p; [containers])" -i runghc

import Data.List (nub, tails)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

inputFile :: FilePath
inputFile = "data/08.txt"

data Grid = Grid
    { coords :: Map Char [(Int, Int)]
    , maxI :: Int
    , maxJ :: Int
    }
    deriving (Show)

parse :: String -> Grid
parse input = Grid cs (pred $ length inputLines) (pred . maximum $ map length inputLines)
  where
    inputLines = lines input
    cs = invertMap . Map.fromList . concat $ zipWith parseLine [0 ..] inputLines
    parseLine y line = [((y, x), c) | (x, c) <- zip [0 ..] line, c /= '.']
    invertMap = Map.foldrWithKey (\coord char -> Map.insertWith (++) char [coord]) Map.empty

run :: String -> (Int, Int)
run s = (part1 grid, part2 grid)
  where
    grid = parse s

part1 :: Grid -> Int
part1 grid = solve grid antinodes
  where
    antinodes ((i1, j1), (i2, j2)) = [(i1 + i, j1 + j), (i2 - i, j2 - j)]
      where
        i = i1 - i2
        j = j1 - j2

part2 :: Grid -> Int
part2 grid = solve grid antinodes
  where
    antinodes ((i1, j1), (i2, j2)) = antinodes' (i1, j1, i, j) ++ antinodes' (i2, j2, negate i, negate j)
      where
        i = i1 - i2
        j = j1 - j2
    antinodes' x@(i, j, i', j') =
        takeWhile (inGrid (maxI grid) (maxJ grid))
            . map getCoord
            $ iterate genAntinodes x
    genAntinodes (i, j, i', j') = (i + i', j + j', i', j')
    getCoord (i, j, _, _) = (i, j)

inGrid :: Int -> Int -> (Int, Int) -> Bool
inGrid iMax jMax (i, j) = i >= 0 && i <= iMax && j >= 0 && j <= jMax

solve :: Grid -> (((Int, Int), (Int, Int)) -> [(Int, Int)]) -> Int
solve grid antinodes =
    length
        . nub
        . filter (inGrid (maxI grid) (maxJ grid))
        . concatMap antinodes
        . concatMap uniquePairs
        . Map.elems
        $ coords grid
  where
    uniquePairs xs = [(x, y) | (x : ys) <- tails xs, y <- ys]

main :: IO ()
main = readFile inputFile >>= print . run
