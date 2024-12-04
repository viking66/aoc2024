#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (p: with p; [extra])" -i runghc

import Data.List (reverse)
import Data.List.Extra ((!?))
import Data.Maybe (catMaybes)

inputFile :: FilePath
inputFile = "data/04.txt"

parse :: String -> [String]
parse = lines

run :: String -> (Int, Int)
run s = (part1 ss, part2 ss)
  where
    ss = parse s

part1 :: [String] -> Int
part1 ss = length . filter (== "XMAS") $ concat [getWords ss (i, j) | i <- is, j <- js]
  where
    is = [0 .. length ss]
    js = [0 .. maximum $ map length ss]

part2 :: [String] -> Int
part2 ss = length $ filter (== XMAS) [tagIdx ss (i, j) | i <- is, j <- js]
  where
    is = [0 .. length ss]
    js = [0 .. maximum $ map length ss]

getCharAt :: [String] -> Int -> Int -> Maybe Char
getCharAt ss x y = ss !? x >>= (!? y)

getWords :: [String] -> (Int, Int) -> [String]
getWords ss (i, j) = catMaybes words
  where
    getChars (x1, y1) (x2, y2) (x3, y3) (x4, y4) = do
        c1 <- getCharAt ss x1 y1
        c2 <- getCharAt ss x2 y2
        c3 <- getCharAt ss x3 y3
        c4 <- getCharAt ss x4 y4
        Just [c1, c2, c3, c4]
    words =
        [ getChars (i, j) (i, j + 1) (i, j + 2) (i, j + 3)
        , getChars (i, j) (i, j - 1) (i, j - 2) (i, j - 3)
        , getChars (i, j) (i + 1, j) (i + 2, j) (i + 3, j)
        , getChars (i, j) (i - 1, j) (i - 2, j) (i - 3, j)
        , getChars (i, j) (i + 1, j + 1) (i + 2, j + 2) (i + 3, j + 3)
        , getChars (i, j) (i - 1, j - 1) (i - 2, j - 2) (i - 3, j - 3)
        , getChars (i, j) (i + 1, j - 1) (i + 2, j - 2) (i + 3, j - 3)
        , getChars (i, j) (i - 1, j + 1) (i - 2, j + 2) (i - 3, j + 3)
        ]

data Tag = XMAS | NoXMAS
    deriving (Eq)

tagIdx :: [String] -> (Int, Int) -> Tag
tagIdx ss (i, j) = case (isXMAS w1, isXMAS w2) of
    (True, True) -> XMAS
    _ -> NoXMAS
  where
    getChars (x1, y1) (x2, y2) (x3, y3) = do
        c1 <- getCharAt ss x1 y1
        c2 <- getCharAt ss x2 y2
        c3 <- getCharAt ss x3 y3
        Just [c1, c2, c3]
    isXMAS (Just w) = w == "MAS" || reverse w == "MAS"
    isXMAS _ = False
    w1 = getChars (i - 1, j - 1) (i, j) (i + 1, j + 1)
    w2 = getChars (i + 1, j - 1) (i, j) (i - 1, j + 1)

main :: IO ()
main = readFile inputFile >>= print . run
