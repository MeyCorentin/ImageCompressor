module Kmeans (kmeans) where

import Data.Char ()
import System.Random
import Control.Monad (replicateM)
import Data.List (minimumBy, transpose)
import Data.Ord (comparing)
import Display (fromLineToRgb, printAssignments)
type Pixel = ((Int, Int), [Int])
type Centroid = [Int]

distance :: [Int] -> [Int] -> Double
distance p q = sqrt (fromIntegral (sum (map (\(x,y) -> (x-y)^2) (zip p q))))

kmeans :: Int -> Double -> String -> IO String
kmeans colors convergence contents = do
    centroides <- sequence (getRandomCentroide colors)
    return (kmeansLoop convergence centroides  (map fromLineToRgb (lines contents)))

kmeansLoop:: Double -> [Centroid] -> [Pixel] -> String
kmeansLoop convergence oldCentroids pixels = do
    let assignCentroides =  ( assignToNearest pixels  oldCentroids)
    let newCentroids = centerCentroide assignCentroides
    if  all (\(old, new) -> distance old new >= convergence) (zip oldCentroids newCentroids)
        then (kmeansLoop convergence newCentroids pixels)
    else
        printAssignments ( assignCentroides)


assignToNearest :: [Pixel] -> [Centroid] -> [(Centroid, [Pixel])]
assignToNearest pixels centroids =
    map (\centroid -> (centroid, filter (\pixel -> nearestCentroid centroids pixel == centroid) pixels)) centroids

nearestCentroid :: [Centroid] -> Pixel -> Centroid
nearestCentroid centroids pixel =  minimumBy (comparing $ distance (snd pixel)) centroids

centerCentroide :: [(Centroid, [Pixel])] -> [Centroid]
centerCentroide assignPixels = do
    (centroid, pixels) <- assignPixels
    let average assignPixels = sum assignPixels `div` length assignPixels
    let center = if null pixels then centroid else map average $ transpose (map snd pixels)
    return center

getRandomCentroide :: Int -> [IO [Int]]
getRandomCentroide 1 = [replicateM 3 (randomRIO (1, 255))]
getRandomCentroide colorsNumber = getRandomCentroide (colorsNumber - 1) ++ [replicateM 3 (randomRIO (1, 255))]