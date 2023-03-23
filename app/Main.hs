module Main (main) where
import System.Environment
import Data.Char (isDigit)

main :: IO ()
main = do
    list <- getArgs :: IO [String]
    putStr(launch list)

launch :: [String] -> String
launch args = do
    imageCompressor colorsNumber convergence path
    where
        colorsNumber = getColor args "-n";
        convergence = getConvergence args "-l";
        path = getPath args "-f";

imageCompressor :: Int -> Float -> String -> String
imageCompressor 1 0.2 "ok" = "working"
imageCompressor colorsNumber convergence path = path


getColor :: [String] -> String -> Int
getColor [] _ = 84
getColor (x:xs) name
    | x == name && not (null xs)                                        =  read (head xs) :: Int
    | otherwise                                                         = getColor  xs name

getConvergence :: [String] -> String -> Float
getConvergence [] _ = 84
getConvergence (x:xs) name
    | x == name && not (null xs)                                        =  read (head xs) :: Float
    | otherwise                                                         = getConvergence  xs name

getPath :: [String] -> String -> String
getPath (x:xs) name
    | x == name && not (null xs)                                        =  read (head xs)
    | otherwise                                                         = getPath  xs name
