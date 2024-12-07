#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (p: with p; [containers parsec])" -i runghc

{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (void)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (foldl', nub, partition)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Text.Parsec (ParseError, char, digit, many1, newline, parse, sepBy)
import Text.Parsec.Char qualified as C
import Text.Parsec.String (Parser)

inputFile :: FilePath
inputFile = "data/05.txt"

data Input = Input
    { rules :: Map Int IntSet
    , validUpdates :: [[Int]]
    , invalidUpdates :: [[Int]]
    }
    deriving (Show)

getRule :: Input -> Int -> IntSet
getRule input n = fromMaybe mempty . Map.lookup n $ rules input

parse' :: String -> Maybe Input
parse' s = case parse inputP "input" s of
    Left _ -> Nothing
    Right input -> Just input

run :: String -> Maybe (Int, Int)
run s = case parse' s of
    Just vals -> Just (part1 vals, part2 vals)
    Nothing -> Nothing

part1 :: Input -> Int
part1 = sum . catMaybes . map middle . validUpdates
  where
    middle xs = case drop (length xs `div` 2) xs of
        [] -> Nothing
        (x : _) -> Just x

part2 :: Input -> Int
part2 input = sum . catMaybes . map middle . map reorder $ invalidUpdates input
  where
    reorder ns = reorder' (IntSet.fromList ns) []
    reorder' remaining updates
        | IntSet.null remaining = reverse updates
        | otherwise =
            let n = next remaining
             in reorder' (IntSet.delete n remaining) (n : updates)
    next ns =
        head'
            . IntSet.toList
            . IntSet.difference ns
            . IntSet.fromList
            . concatMap (IntSet.toList . getRule input)
            $ IntSet.toList ns
    head' (x : _) = x
    head' _ = error "empty list"
    middle xs = case drop (length xs `div` 2) xs of
        [] -> Nothing
        (x : _) -> Just x

numberP :: Parser Int
numberP = read <$> many1 digit

ruleP :: Parser (Int, Int)
ruleP = do
    n1 <- numberP
    char '|'
    n2 <- numberP
    newline
    pure (n1, n2)

updateP :: Parser [Int]
updateP = do
    nums <- numberP `sepBy` char ','
    newline
    pure nums

inputP :: Parser Input
inputP = do
    rules <- allRulesP
    newline
    updates <- allUpdatesP
    let ruleSet = toMap rules
    let (valid, invalid) = partition (isUpdateValid ruleSet) updates
    pure $ Input (toMap rules) valid invalid
  where
    allRulesP = many1 ruleP
    allUpdatesP = many1 updateP
    toMap = foldr insert Map.empty
    insert (a, b) = Map.insertWith IntSet.union a (IntSet.singleton b)
    isUpdateValid :: Map Int IntSet -> [Int] -> Bool
    isUpdateValid rules = fst . foldl' f (True, mempty)
      where
        f (valid, seen) n = (valid && isValid seen n, IntSet.insert n seen)
        isValid seen n =
            IntSet.null
                . IntSet.intersection seen
                . fromMaybe mempty
                $ Map.lookup n rules

main :: IO ()
main = do
    input <- readFile inputFile
    case run input of
        Just n -> print n
        Nothing -> putStrLn "Something went wrong"
