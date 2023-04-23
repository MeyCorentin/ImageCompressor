--
-- EPITECH PROJECT, 2023
-- Display
-- File description:
-- Display
--

module Display (fromLineToRgb, printAssignments) where


type Pixel = ((Int, Int), [Int])
type Centroid = [Int]

removeParentheses :: String -> String
removeParentheses str = filter (\c -> c /= '(' && c /= ')') str

parseRGB :: String -> [Int]
parseRGB str = read $ "[" ++ str ++ "]" :: [Int]

parseRPOS :: String -> (Int, Int)
parseRPOS str =
    read $ "(" ++ (filter (/=')') . filter (/='(')) str ++ ")" :: (Int, Int)

fromLineToRgb :: String -> Pixel
fromLineToRgb str = ((parseRPOS (removeParentheses (head (words str)))),
    (parseRGB (removeParentheses (last (words str)))))

printAssignments :: [(Centroid, [Pixel])] -> String
printAssignments assignments =
            concatMap (\(centroid, pixels) ->
                "--\n" ++ (\[r,g,b] -> "(" ++ show r ++ "," ++ show g ++ "," ++
                show b ++ ")") centroid ++ "\n-\n" ++
                concatMap (\((x,y), [r,g,b]) ->
                    "(" ++ show x ++ "," ++ show y ++ ") (" ++ show r ++ "," ++
                    show g ++ "," ++ show b ++ ")\n")
                pixels
            ) assignments
