#! /usr/bin/env nix-shell
#! nix-shell -p "haskellPackages.ghcWithPackages (p: with p; [parsec])" -i runghc

import Data.Maybe (catMaybes)
import Text.Parsec (
    anyToken,
    char,
    digit,
    many,
    many1,
    parse,
    string,
    try,
    (<|>),
 )
import Text.Parsec.String (Parser)

inputFile :: FilePath
inputFile = "data/03.txt"

data Mul = Mul Int Int
    deriving (Show)

data Inst = MulI Mul | DoI | Don'tI
    deriving (Show)

parse' :: String -> Maybe [Inst]
parse' s = case parse allInstP "(unknown)" s of
    Left _ -> Nothing
    Right xs -> Just xs

run :: String -> Maybe (Int, Int)
run s = case parse' s of
    Just insts -> Just (part1 insts, part2 insts)
    Nothing -> Nothing

part1 :: [Inst] -> Int
part1 = sum . map mul' . catMaybes . map getMul
  where
    getMul (MulI m) = Just m
    getMul _ = Nothing

part2 :: [Inst] -> Int
part2 = go True
  where
    go _ [] = 0
    go processing (inst : rest) = case inst of
        DoI -> go True rest
        Don'tI -> go False rest
        MulI m -> (if processing then mul' m else 0) + go processing rest

mul' :: Mul -> Int
mul' (Mul x y) = x * y

mulP :: Parser Mul
mulP = Mul <$> (string "mul(" *> intP <* char ',') <*> (intP <* char ')')

intP :: Parser Int
intP = read <$> many1 digit

instP :: Parser Inst
instP =
    try (Don'tI <$ string "don't")
        <|> (DoI <$ string "do")
        <|> (MulI <$> mulP)

allInstP :: Parser [Inst]
allInstP = catMaybes <$> many parseElement
  where
    validInst = Just <$> instP
    skipChar = anyToken >> return Nothing
    parseElement = try validInst <|> skipChar

main :: IO ()
main = do
    input <- readFile inputFile
    case run input of
        Just n -> print n
        Nothing -> putStrLn "Something went wrong"
