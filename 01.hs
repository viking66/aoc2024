#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (p: with p; [safe])" -i runghc

{-# LANGUAGE OverloadedStrings #-}

import Data.List (sort)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Safe (readMay)

inputFile :: FilePath
inputFile = "data/01.txt"

parse :: String -> Maybe ([Int], [Int])
parse = fmap toLists . traverse toPair . lines
  where
    toPair :: String -> Maybe (Int, Int)
    toPair s = (traverse readMay $ words s) >>= toTuple
    toTuple [a, b] = Just (a, b)
    toTuple _ = Nothing
    toLists xs = (sort $ map fst xs, sort $ map snd xs)

run :: String -> Maybe (Int, Int)
run s = case parse s of
    Just vals -> Just (part1 vals, part2 vals)
    Nothing -> Nothing

part1 :: ([Int], [Int]) -> Int
part1 (xs, ys) = sum $ zipWith diff xs ys
  where
    diff x y = abs $ x - y

part2 :: ([Int], [Int]) -> Int
part2 (xs, ys) = sum $ map score xs
  where
    counts = map (\as -> (NonEmpty.head as, NonEmpty.length as)) $ NonEmpty.group ys
    score n = (n *) . fromMaybe 0 $ lookup n counts

main :: IO ()
main = do
    input <- readFile inputFile
    case run input of
        Just n -> print n
        Nothing -> putStrLn "Something went wrong"
