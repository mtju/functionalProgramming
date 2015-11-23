module SxprList
where
import Data.Char
import Data.List
import TTTcommon

parseBoard :: String -> Board
parseBoard str = parseBoardFields $ splitEvery 8 $ removeNonEssentialStuff $ removeOuter str

removeOuter :: String -> String
removeOuter str = 
    let
        parenth1 = takeWhile (/= '(') str
        rest1 = drop (length parenth1 + 1) str
        parenth2 = takeWhile (/= '"') rest1
        rest2 = drop (length parenth2) rest1
        --- Remove last parenthese
        parenth3 = takeWhile (/= ')') (reverse rest2)
        withoutLast = reverse (drop (length parenth3 + 1) (reverse rest2))
    in withoutLast

removeNonEssentialStuff :: String -> String
removeNonEssentialStuff str = filter (/= '(') $ filter (/= ')') $ filter (/= '"') $ filter (/= '\\') $ filter (/= ' ') str

-- Split at 7 --
splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first, rest) = splitAt n list

parseBoardField :: String -> BoardField
parseBoardField str = 
    let
        x = digitToInt $ str !! 3
        y = digitToInt $ str !! 5
        v = str !! 7
    in (x, y, v)

parseBoardFields :: [String] -> Board
parseBoardFields [] = []
parseBoardFields (x:xs) = parseBoardField(x) : parseBoardFields xs

exportBoard :: Board -> String
exportBoard board = "(m" ++ (exportBoardFields board "" 0) ++ ")"

exportBoardFields :: Board -> String -> Int -> String
exportBoardFields [] str _ = str
exportBoardFields (field : left) str counter = let
    strField = "\"" ++ (show counter) ++ "\" " ++ (exportField field)
    in exportBoardFields left (str ++ " " ++ strField) (counter + 1)

exportField :: BoardField -> String
exportField (x, y, v) = "(m \"x\" " ++ (show x) ++ " \"y\" " ++ (show y) ++ " \"v\" \"" ++ [v] ++ "\")"

