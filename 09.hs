#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (p: with p; [])" -i runghc
{-# LANGUAGE LambdaCase #-}

import Data.Char (chr, ord)
import Data.List (foldl')
import Data.Maybe (fromMaybe)

inputFile :: FilePath
inputFile = "data/09.txt"

data Mem = File Int Int | FreeSpace Int
    deriving (Eq, Show)

isFile :: Mem -> Bool
isFile (File _ _) = True
isFile _ = False

isFree :: Mem -> Bool
isFree (FreeSpace _) = True
isFree _ = False

size :: Mem -> Int
size (File _ n) = n
size (FreeSpace n) = n

alloc :: Mem -> Mem -> (Maybe Mem, Maybe Mem, Maybe Mem)
alloc free file = (allocFile, remainingFreeSpace, remainingFileSpace)
  where
    allocInfo = case (free, file) of
        (FreeSpace m, File idx n) ->
            let allocSize = min m n
                remainingFreeSize = m - allocSize
                remainingFileSize = n - allocSize
             in Just (allocSize, remainingFreeSize, remainingFileSize, idx)
        (_, File idx n) -> Just (0, 0, n, idx)
    allocFile =
        allocInfo >>= \(size, _, _, idx) -> if size == 0 then Nothing else Just (File idx size)
    remainingFreeSpace =
        allocInfo >>= \(_, size, _, _) -> if size == 0 then Nothing else Just (FreeSpace size)
    remainingFileSpace =
        allocInfo >>= \(_, _, size, idx) -> if size == 0 then Nothing else Just (File idx size)

parse :: String -> [Mem]
parse s = zipWith3 (\idx size f -> f idx size) idxs digits (cycle [File, free])
  where
    idxs = concatMap (replicate 2) [0 ..]
    digits = map (\c -> fromEnum c - fromEnum '0') s
    free _ size = FreeSpace size

run :: String -> (Int, Int)
run s = (part1 mem, part2 mem)
  where
    mem = parse s

part1 :: [Mem] -> Int
part1 mem =
    sum
        . zipWith (*) [0 ..]
        . take len
        . concatMap expand
        $ merge mem mem'
  where
    charToDigit c = fromEnum c - fromEnum '0'
    mem' = filter isFile $ reverse mem
    len = sum $ map size mem'
    merge [] bs = bs
    merge as [] = as
    merge (a : as) (b : bs) = c : merge as' bs'
      where
        (allocFile, remainingFreeSpace, remainingFileSpace) = alloc a b
        c = fromMaybe a allocFile
        as' = maybe as (: as) remainingFreeSpace
        bs' = maybe bs (: bs) remainingFileSpace
    expand = \case
        File idx len -> replicate len idx
        FreeSpace len -> replicate len 0

part2 :: [Mem] -> Int
part2 mem = sum . zipWith (*) [0 ..] . concatMap expand $ foldl' move mem files
  where
    foo = foldl' move mem files
    files :: [Mem]
    files = filter isFile $ reverse mem

    move :: [Mem] -> Mem -> [Mem]
    move mem' f = compact . tryAlloc f $ freeFile mem' f

    freeFile :: [Mem] -> Mem -> [Mem]
    freeFile mem' m = freeFile' mem' m

    freeFile' :: [Mem] -> Mem -> [Mem]
    freeFile' mem' = \case
        f@(File _ n) -> map (\m -> if m == f then FreeSpace n else m) mem'
        _ -> mem'

    tryAlloc :: Mem -> [Mem] -> [Mem]
    tryAlloc (FreeSpace _) mem' = mem'
    tryAlloc _ [] = []
    tryAlloc f@(File idx n) (fs@(FreeSpace m) : mem') =
        if m >= n
            then let ms = tryAlloc' idx n fs in ms ++ mem'
            else fs : tryAlloc f mem'
    tryAlloc f (m : mem') = m : tryAlloc f mem'

    tryAlloc' :: Int -> Int -> Mem -> [Mem]
    tryAlloc' idx n = \case
        x@(File _ _) -> [x]
        y@(FreeSpace m) -> case compare m n of
            LT -> [y]
            EQ -> [File idx n]
            GT -> [File idx n, FreeSpace (m - n)]

    compact :: [Mem] -> [Mem]
    compact (x : y : zs) = case (x, y) of
        (FreeSpace m, FreeSpace n) -> compact $ (FreeSpace $ m + n) : zs
        _ -> x : compact (y : zs)
    compact xs = xs
    expand = \case
        File idx len -> replicate len idx
        FreeSpace len -> replicate len 0

main :: IO ()
main = readFile inputFile >>= print . run
