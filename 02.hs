#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (p: with p; [safe])" -i runghc

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.List (sort)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Safe (readMay)

inputFile :: FilePath
inputFile = "data/02.txt"

parse :: String -> Maybe [[Int]]
parse = traverse toIntList . lines
  where
    toIntList = traverse readMay . words

run :: String -> Maybe (Int, Int)
run s = case parse s of
    Just vals -> Just $ (part1 vals, part2 vals)
    Nothing -> Nothing

part1 :: [[Int]] -> Int
part1 = length . filter isSafe

part2 :: [[Int]] -> Int
part2 = length . filter anyIsSafe
  where
    anyIsSafe = any isSafe . removeOneElement

removeOneElement :: [a] -> [[a]]
removeOneElement [] = []
removeOneElement [x] = [[]]
removeOneElement (x : xs) = xs : map (x :) (removeOneElement xs)

isSafe :: [Int] -> Bool
isSafe xs = isSafe' xs || isSafe' (reverse xs)
  where
    isSafe' xs =
        let gs = gaps xs
         in minimum gs >= 1 && maximum gs <= 3

gaps :: [Int] -> [Int]
gaps = \case
    [] -> []
    xs -> zipWith (-) (tail xs) xs

main :: IO ()
main = do
    input <- readFile inputFile
    case run input of
        Just n -> print n
        Nothing -> putStrLn "Something went wrong"
