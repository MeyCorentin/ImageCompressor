module Main (main) where
import System.Environment
import Data.Char ()

import Text.Read (readMaybe)

main :: IO ()
-- *récupère les arguments et lance la function launch
main = do
    list <- getArgs :: IO [String]
    launch list >>= putStr

launch :: [String] -> IO String
-- *lance la function image compressor en récupérant les arguments souhaité
launch args = do
    imageCompressor n l f
    where
        n = readColorsNumber args
        l = readConvergence args
        f = readPath args


imageCompressor :: Maybe Int -> Maybe Float -> Maybe String -> IO String
-- *regarde si les arguments ne sont pas des nothing
-- * et lance la function readFile
imageCompressor Nothing _ _ = return "erreur"
imageCompressor _ Nothing _ = return "erreur"
imageCompressor _ _ Nothing = return "erreur"
imageCompressor (Just n) (Just l) (Just f) = do
    contents <- myReadFile (Just f)
    case contents of
        Just c -> return c
        Nothing -> return "Le fichier n'a pas été trouvé."

myReadFile :: Maybe String -> IO (Maybe String)
-- * retourne le contenue du file graçe au path donné en arguments
myReadFile Nothing = return Nothing
myReadFile (Just path) = do
    contents <- readFile path
    return (Just contents)


getMyArgs :: Maybe [String] -> String -> Maybe String
-- * récupère les arguments en comparant la string passé en paramètre
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