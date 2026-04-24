module Main (main) where

import Control.Monad ((>=>))
import Text.Read (readMaybe)

main :: IO ()
main = getContents >>= putStrLn . output . (parse >=> eval)

data Token = Plus | Minus | Mult | Div | Value Double

instance Show Token where
  show Plus = "+"
  show Minus = "-"
  show Mult = "*"
  show Div = "/"
  show (Value v) = show v

parse :: String -> Either String [Token]
parse s = mapM parseToken (words s)

parseToken :: String -> Either String Token
parseToken "+" = Right Plus
parseToken "-" = Right Minus
parseToken "*" = Right Mult
parseToken "/" = Right Div
parseToken n = case readMaybe n of
  Just number -> Right $ Value number
  Nothing -> Left ("invalid token: " ++ n)

eval :: [Token] -> Either String String
eval toks = eval' toks []

eval' :: [Token] -> [Double] -> Either String String
eval' (Plus : toks) (b : a : vals) = eval' toks (a + b : vals)
eval' (Minus : toks) (b : a : vals) = eval' toks (a - b : vals)
eval' (Mult : toks) (b : a : vals) = eval' toks (a * b : vals)
eval' (Div : toks) (b : a : vals) = eval' toks (a / b : vals)
eval' (Value v : toks) vals = eval' toks (v : vals)
eval' [] [a] = Right $ "evaluated " ++ show a
eval' [] [] = Right "empty"
eval' a b = Left $ "unable to reduce: " ++ show a ++ show b

output :: Either String String -> String
output (Left s) = "Error: " ++ s
output (Right s) = s
