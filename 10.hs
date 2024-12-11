#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (p: with p; [containers])" -i runghc

import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set

type Position = (Int, Int, Int) -- (height, row, col)

parse :: String -> Map Int [(Int, Int)]
parse = Map.fromListWith (++) . concat . zipWith handleLine [0 ..] . lines
  where
    handleLine row = zipWith (mkResult row) [0 ..]
    mkResult row col char = ((read [char], [(row, col)]))

solve
    :: (Position -> a) -- How to score a 9
    -> (Position -> [(Position, a)] -> a) -- How to combine neighbor scores
    -> (Position -> a -> Int) -- How to convert a score to Int
    -> Map Int [(Int, Int)] -- Input grid
    -> Int -- Final result
solve baseScore combineScores convertScore m = sumZeroScores m convertScore finalScores
  where
    finalScores = foldl' insertScore mempty . concatMap expand . Map.toDescList $ m
    expand (n, ps) = [(n, i, j) | (i, j) <- ps]
    insertScore scores pos = Map.insert pos (getScore scores pos) scores
    getScore scores pos@(9, _, _) = baseScore pos
    getScore scores pos@(n, i, j) =
        combineScores
            pos
            [ (npos, fromMaybe (error "Invalid state") $ Map.lookup npos scores)
            | (ni, nj) <- getValidNeighbors m (n + 1) (i, j)
            , let npos = (n + 1, ni, nj)
            ]
    getValidNeighbors m height (i, j) =
        [ (ni, nj)
        | (ni, nj) <- [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]
        , (ni, nj) `elem` (fromMaybe [] $ Map.lookup height m)
        ]
    sumZeroScores m convertScore scores =
        sum
            [ convertScore (0, i, j) $
                fromMaybe (error "Invalid state") $
                    Map.lookup (0, i, j) scores
            | (i, j) <- fromMaybe [] $ Map.lookup 0 m
            ]

part1 :: Map Int [(Int, Int)] -> Int
part1 =
    solve
        (\pos -> Set.singleton pos) -- A 9 can reach itself
        (\_ neighbors -> Set.unions (map snd neighbors)) -- Union all reachable 9s
        (\_ set -> Set.size set) -- Convert set size to Int

part2 :: Map Int [(Int, Int)] -> Int
part2 =
    solve
        (const 1) -- A 9 has one path
        (\_ neighbors -> sum (map snd neighbors)) -- Sum paths from neighbors
        (\_ n -> n) -- Score is already an Int

run :: String -> (Int, Int)
run s = (part1 m, part2 m)
  where
    m = parse s

main :: IO ()
main = readFile "data/10.txt" >>= print . run
