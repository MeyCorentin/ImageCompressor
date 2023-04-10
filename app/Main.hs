module Main (main) where

import System.Environment
import Parser (readColorsNumber, readConvergence, readPath, myReadFile)
import Kmeans (kmeans)

main :: IO ()
main = do
    list <- getArgs :: IO [String]
    result <- launch list
    putStrLn (result)

launch :: [String] -> IO String
launch args = do
    imageCompressor n l f
    where
        n = readColorsNumber args
        l = readConvergence args
        f = readPath args

imageCompressor :: Maybe Int -> Maybe Double -> Maybe String -> IO String
imageCompressor Nothing _ _ = return "erreur"
imageCompressor _ Nothing _ = return "erreur"
imageCompressor _ _ Nothing = return "erreur"
imageCompressor (Just n) (Just l) (Just f) = do
    contents <- myReadFile (Just f)
    case contents of
        Just c ->  (kmeans n l c)
        Nothing -> return "Le fichier n'a pas été trouvé."
