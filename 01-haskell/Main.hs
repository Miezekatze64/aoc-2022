module Main where

import Data.List
import Data.Maybe

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn x xs = take index xs : splitOn x (drop (index+1) xs)
  where
    index = fromMaybe (length xs) $ elemIndex x xs

main :: IO ()
main = do
  nums <- map (sum . map read) . splitOn "" . lines <$> readFile "input.txt" :: IO [Int]
  putStrLn $ "Part 1: " ++ show ( maximum nums )
  putStrLn $ "Part 2: " ++ show ( sum $ take 3 $ reverse $ sort nums )

