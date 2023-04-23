--
-- EPITECH PROJECT, 2023
-- Parser
-- File description:
-- Parser
--

module Parser (readColorsNumber, readConvergence, readPath, myReadFile) where

import Text.Read (readMaybe)

myReadFile :: Maybe String -> IO (Maybe String)
myReadFile Nothing = return Nothing
myReadFile (Just path) = do
    contents <- readFile path
    return (Just contents)

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

readConvergence :: [String] -> Maybe Double
readConvergence args = do
    arg <- getMyArgs (Just args) "-l"
    readMaybe arg

readPath :: [String] -> Maybe String
readPath args = do
    arg <- getMyArgs (Just args) "-f"
    Just arg
