module Main (main) where
import System.Environment
import Data.Char ()
import System.Random
import Control.Monad (replicateM)

import Text.Read (readMaybe)

main :: IO ()
main = do
    list <- getArgs :: IO [String]
    launch list >>= putStr

launch :: [String] -> IO String
-- * Prend : les arguments récupérer
-- * Retourne : le résultat de la function imageCompressor
launch args = do
    imageCompressor n l f
    where
        n = readColorsNumber args
        l = readConvergence args
        f = readPath args


imageCompressor :: Maybe Int -> Maybe Float -> Maybe String -> IO String
-- * Prend : Un nombre de couleur une valeur de convergence et le path du file
-- * Retourne : le résultat du kmeans algorithme
imageCompressor Nothing _ _ = return "erreur"
imageCompressor _ Nothing _ = return "erreur"
imageCompressor _ _ Nothing = return "erreur"
imageCompressor (Just n) (Just l) (Just f) = do
    contents <- myReadFile (Just f)
    case contents of
        Just c ->  (kmeans n l c)
        Nothing -> return "Le fichier n'a pas été trouvé."


-- ? [File reading] --

myReadFile :: Maybe String -> IO (Maybe String)
-- * Prend : un path
-- * Rtourne : le contenue du file donnée en arguments
myReadFile Nothing = return Nothing
myReadFile (Just path) = do
    contents <- readFile path
    return (Just contents)

-- ? [ARGS PARSING] --

getMyArgs :: Maybe [String] -> String -> Maybe String
getMyArgs Nothing _ = Nothing
getMyArgs (Just []) _ = Nothing
getMyArgs (Just (x:xs)) name
    | x == name && not (null xs)    = Just (head xs)
    | otherwise                     = getMyArgs (Just xs) name


readColorsNumber :: [String] -> Maybe Int
readColorsNumber args = do
    arg <- getMyArgs (Just args) "-n"
    readMaybe arg

readConvergence :: [String] -> Maybe Float
readConvergence args = do
    arg <- getMyArgs (Just args) "-l"
    readMaybe arg

readPath :: [String] -> Maybe String
readPath args = do
    arg <- getMyArgs (Just args) "-f"
    Just arg

-- ? [KMEANS algorithm] --

-- TODO allocatePixel:: [IO [Int]] -> [String] -> [[IO [Int], [String]]]
-- ! Prend  : un entrer une liste de centroide et le contenue du file
-- ! Retourne : une liste contenant au début le centoide et a la fin les pixels lui appartenant
-- allocatePixel centroides contents

rgbRandom :: IO [Int]
-- * Retourne : un tripple de 3 int correspondant a des valeurs rgb
-- ! IO [INT] car random donc effet de bord
rgbRandom = replicateM 3 (randomRIO (1, 255))

getRandomCentroide :: Int -> [IO [Int]]
-- * Prend : le nombre de couleurs souhaités
-- * Retourne : des couleurs sous le format [(int, int, int)] du nombre de couleurs
getRandomCentroide 1 = [rgbRandom]
getRandomCentroide colorsNumber = getRandomCentroide (colorsNumber - 1) ++ [rgbRandom]

kmeansLoop:: Float -> [ IO [Int]] -> String -> IO String
-- * Prend : la convergence souhaié, une liste de centroide et le contenue du file
-- * Retourne : le résultat de l'algorithme jusqua la convergence souhaité
kmeansLoop _ centroides _ = do 
    results <- sequence centroides
    return (show results)
-- kmeansLoop convergence oldCentroide contents = do
--     newCentroide -> centerCentroide (allocatePixel oldCentroide  (lines contents))
--     if ( convergence >= calculConvergece newCentroide oldCentroide)
--         return (listToString (allocatePixel oldCentroide  (lines contents)))
--     else
--         kmeansLoop convergence newCentroide contents


-- TODO : listToString :: [[Int, [String]]] -> String
-- ! Prend : le tableau contenant les centroides et les pixels qui leurs sont attribué
-- ! Retourne : une chaine de charactère du format souhaité
-- x! utiliser show et foldl

-- TODO : calculConvergece:: [[Int]] -> [[Int]] -> Float
-- ! Prend : les anciens et les nouveaux centroïde
-- ! Retourne : la convergence des deux

-- TODO : centerCentroide :: [[Int, [String]]] -> [[Int]]
-- ! Prend : les centroïdes et les pixels qui leurs sont attribués
-- ! Retourne : les centroïdes centrés sur les pixels attribué 

kmeans :: Int -> Float -> String -> IO String
-- * Prend : Un nombre de couleur une valeur de convergence et le path du file
-- * Retourne : le résultat de la boucle le résultat de la boucle kmeansLoop
kmeans colors convergence contents = kmeansLoop convergence (getRandomCentroide colors) contents