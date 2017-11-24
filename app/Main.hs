module Main where

import Lib
import Text.CSV
import Text.Parsec.Error

noEmptyRows :: Either ParseError CSV -> CSV
noEmptyRows = either (const []) (filter (\row -> 2 <= length row))

readIndex :: Read cell => Either ParseError CSV  -> Int-> [cell]
readIndex csv index = map (read . (!! index)) (noEmptyRows csv)

baseball :: IO (Either ParseError CSV)
baseball = parseCSVFromFile "./data/GL2015.TXT"

awayRuns :: IO [Int] 
awayRuns = baseball >>= return . (flip readIndex $ 9)

range :: Ord a => [a] -> Maybe (a, a)
range [] = Nothing
range xs = Just (minimum xs, maximum xs)

main :: IO ()
main = do
   a <- awayRuns
   putStrLn $ show $ range a

