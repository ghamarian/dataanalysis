module Main where

import Lib
import Text.CSV
import Text.Parsec.Error
import Data.Maybe
import Data.List
import Control.Applicative
import Data.Ord
import Data.Time (getCurrentTime)

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

mean :: Real a => [a] -> Maybe Double
mean [] = Nothing
mean [x] = Just $ realToFrac x
mean xs = Just $ (realToFrac $ sum xs) / fromIntegral (length xs)

stdev :: Real a => [a] -> Maybe Double
stdev [] = Nothing
stdev [_] = Nothing
stdev xs = Just $ sqrt (sumsqr / fromIntegral (length xs - 1)) where
         meanx = fromJust $ mean xs
         sumsqr = sum $ map (diffsquare .  realToFrac) xs
         diffsquare x = (meanx - x) * (meanx - x)

median :: Real a => [a] -> Maybe Double
median [] = Nothing
median xs | len `mod` 2 == 1 = Just $ mid
          | otherwise = Just $  (mid + midprev) / 2
          where
            midIndex = len `div` 2
            mid = realToFrac $ sorted !! midIndex
            midprev = realToFrac $ sorted !! (midIndex - 1)
            sorted = sort xs
            len = length xs

modCalc [] = Nothing
modCalc xs = Just $ head $ maximumBy (comparing length) $ group $ sort xs

main :: IO ()
main = do
   a <- awayRuns
   putStrLn $ show $ range a
   print $ mean a
   print $ stdev a
   print $ median a
   print $ modCalc a
   time <- getCurrentTime
   print $ show time 


