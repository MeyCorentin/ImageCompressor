{-
-- EPITECH PROJECT, 2023
-- Kmeans
-- File description:
-- Kmeans
-}

module Kmeans (kmeans) where

import Control.Monad (replicateM)
import Data.List (foldl', minimumBy, transpose)
import Data.Ord (comparing)
import Data.Function ((&))
import Display (fromLineToRgb, printAssignments)
import System.Random (Random, randomRIO)
type Pixel = ((Int, Int), [Int])
type Centroid = [Int]

distance :: [Int] -> [Int] -> Double
distance p q = sqrt (fromIntegral (sum (map (\(x,y) -> (x-y)^2) (zip p q))))

kmeans :: Int -> Double -> String -> IO String
kmeans colors conv contents = do
    centroides <- replicateM colors (replicateM 3 (randomRIO (1, 255)))
    return (kmeansLoop conv centroides(map fromLineToRgb(lines contents)))

kmeansLoop :: Double -> [Centroid] -> [Pixel] -> String
kmeansLoop convergence oldCentroids pixels =
    let assignCentroides = assignToNearest pixels oldCentroids
        newCentroids = centerCentroide assignCentroides
    in if all (\(old, new) -> distance old new >= convergence)
             (zip oldCentroids newCentroids)
       then kmeansLoop convergence newCentroids pixels
       else printAssignments assignCentroides

assignToNearest :: [Pixel] -> [Centroid] -> [(Centroid, [Pixel])]
assignToNearest pixels centroids =
    map (\centroid -> (centroid, filter
    (\pixel -> nearestCentroid centroids pixel == centroid) pixels)) centroids

nearestCentroid :: [Centroid] -> Pixel -> Centroid
nearestCentroid centroids pixel =
    minimumBy (comparing $ distance (snd pixel)) centroids

centerCentroide :: [(Centroid, [Pixel])] -> [Centroid]
centerCentroide assignPixels = do
    (centroid, pixels) <- assignPixels
    let average = foldl' (\acc x -> zipWith (+) acc x) (replicate 3 0)
            (map snd pixels) & map (\x -> x `div` length pixels)
    let center = if null pixels then centroid else average
    return center
