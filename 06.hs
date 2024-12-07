#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (p: with p; [containers])" -i runghc
{-# LANGUAGE LambdaCase #-}

import Data.List (unfoldr)
import Data.Set (Set)
import Data.Set qualified as Set
import Debug.Trace (trace)

inputFile :: FilePath
inputFile = "data/06.txt"

data Input = Input
    { guard :: Guard
    , grid :: Set (Int, Int)
    , minI :: Int
    , maxI :: Int
    , minJ :: Int
    , maxJ :: Int
    }
    deriving (Show)

data Dir = U | D | L | R
    deriving (Eq, Ord, Show)

data Guard = Guard Dir (Int, Int)
    deriving (Eq, Ord, Show)

data Contents = Empty | Full | OffGrid
    deriving (Show)

atCoord :: Input -> (Int, Int) -> Contents
atCoord input (i, j)
    | i < minI input = OffGrid
    | i > maxI input = OffGrid
    | j < minJ input = OffGrid
    | j > maxJ input = OffGrid
    | Set.member (i, j) (grid input) = Full
    | otherwise = Empty

move :: Input -> Maybe Input
move input = case atCoord input c of
    Empty -> Just input{guard = g'}
    Full -> move input{guard = rotate g}
    OffGrid -> Nothing
  where
    g = guard input
    g'@(Guard _ c) = step g

inLoop :: Input -> Bool
inLoop = go mempty
  where
    go seen input = case move input of
        Nothing -> False
        Just input' ->
            let g = guard input'
             in if Set.member g seen
                    then True
                    else go (Set.insert g seen) input'

gCoord :: Input -> (Int, Int)
gCoord = (\(Guard _ c) -> c) . guard

step :: Guard -> Guard
step (Guard dir (i, j)) = Guard dir $ case dir of
    U -> (i - 1, j)
    D -> (i + 1, j)
    L -> (i, j - 1)
    R -> (i, j + 1)

rotate :: Guard -> Guard
rotate (Guard dir coord) = Guard dir' coord
  where
    dir' = case dir of
        U -> R
        D -> L
        L -> U
        R -> D

parse :: String -> Maybe Input
parse s = mkInput <$> getGuard
  where
    ss = lines s
    i = length ss - 1
    j = (maximum $ map length ss) - 1
    coords = [(i', j') | i' <- [0 .. i], j' <- [0 .. j]]
    gridCoords = zip (concat ss) coords
    getGuard = case filter ((`elem` "^v<>") . fst) gridCoords of
        [(dir, coord)] -> (\d -> Guard d coord) <$> getDir dir
        _ -> Nothing
    getDir = \case
        '^' -> Just U
        'v' -> Just D
        '<' -> Just L
        '>' -> Just R
        _ -> Nothing
    mkInput guard =
        Input
            { guard = guard
            , grid = Set.fromList . map snd $ filter ((== '#') . fst) gridCoords
            , minI = 0
            , maxI = i
            , minJ = 0
            , maxJ = j
            }

run :: String -> Maybe (Int, Int)
run s = case parse s of
    Just input -> Just (part1 input, part2 input)
    Nothing -> Nothing

visited :: Input -> Set (Int, Int)
visited = Set.fromList . map gCoord . unfoldr f
  where
    f i = case move i of
        Nothing -> Nothing
        Just i' -> Just (i, i')

part1 :: Input -> Int
part1 = succ . Set.size . visited

-- This is super slow and should be optimized
part2 :: Input -> Int
part2 input = succ . length $ filter f vs
  where
    vs = Set.toList $ visited input
    f c = inLoop input{grid = Set.insert c (grid input)}

main :: IO ()
main = do
    input <- readFile inputFile
    case run input of
        Just n -> print n
        Nothing -> putStrLn "Something went wrong"
