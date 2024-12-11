#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (p: with p; [memoize])" -i runghc

import Data.Function.Memoize (memoFix2)
import Data.List (foldl')

inputFile :: FilePath
inputFile = "data/11.txt"

parse :: String -> [Int]
parse = map read . words

run :: String -> (Int, Int)
run s = (part1 ns, part2 ns)
  where
    ns = parse s

part1 :: [Int] -> Int
part1 = countStones 25

part2 :: [Int] -> Int
part2 = countStones 75

countStones :: Int -> [Int] -> Int
countStones n = sum . map (memoBlink n)
  where
    memoBlink = memoFix2 blink
    blink :: (Int -> Int -> Int) -> (Int -> Int -> Int)
    blink f n stone
        | n == 0 = 1
        | stone == 0 = f n' 1
        | even (length s) = f n' l + f n' r
        | otherwise = f n' (stone * 2024)
      where
        n' = n - 1
        s = show stone
        (l, r) = let (l', r') = splitAt (length s `div` 2) s in (read l', read r')

main :: IO ()
main = readFile inputFile >>= print . run
