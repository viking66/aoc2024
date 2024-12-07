#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (p: with p; [split])" -i runghc

import Data.List.Split (splitOn)

inputFile :: FilePath
inputFile = "data/07.txt"

data Equation = Equation Int [Int]

target :: Equation -> Int
target (Equation n _) = n

parse :: String -> [Equation]
parse = map toEquation . lines
  where
    toEquation s = case splitOn ": " s of
        [n, ns] -> Equation (read n) (map read $ words ns)
        _ -> error "bad input"

run :: String -> (Int, Int)
run s = (part1 es, part2 es)
  where
    es = parse s

part1 :: [Equation] -> Int
part1 = go [(+), (*)]

-- This is slow and should be optimized
part2 :: [Equation] -> Int
part2 = go [(+), (*), (|||)]
  where
    a ||| b = read $ show a ++ show b

go :: [Int -> Int -> Int] -> [Equation] -> Int
go os = sum . map target . filter f
  where
    combinations :: [a] -> Int -> [[a]]
    combinations xs n = sequence $ replicate n xs
    applyOps :: [a] -> [(a -> a -> a)] -> a
    applyOps (x : xs) ops = foldl (\acc (op, y) -> op acc y) x (zip ops xs)
    allOps n = combinations os n
    f (Equation n ns) = any (\op -> applyOps ns op == n) (allOps $ length ns - 1)

main :: IO ()
main = readFile inputFile >>= print . run
