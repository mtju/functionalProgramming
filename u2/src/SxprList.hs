module SxprList
where
import Data.Char
import Data.List
import TTTcommon

parseBoard :: String -> Board
parseBoard str = parseBoardFields $ splitEvery 7 $ removeNonEssentialStuff $ removeL str

removeL :: String -> String
removeL str = 
    let
        parenth1 = takeWhile (/= '(') str
        rest1 = drop (length parenth1 + 1) str
        parenth2 = takeWhile (/= '(') rest1
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
        x = digitToInt $ str !! 2
        y = digitToInt $ str !! 4
        v = str !! 6
    in (x, y, v)

parseBoardFields :: [String] -> Board
parseBoardFields [] = []
parseBoardFields (x:xs) = parseBoardField(x) : parseBoardFields xs

exportBoard :: Board -> String
exportBoard board = "(l" ++ (exportBoardFields board "") ++ ")"

exportBoardFields :: Board -> String -> String
exportBoardFields [] str = str
exportBoardFields (field : left) str = let
    strField = exportField field
    in exportBoardFields left (str ++ " " ++ strField)

exportField :: BoardField -> String
exportField (x, y, v) = "(m \"x\" " ++ (show x) ++ " \"y\" " ++ (show y) ++ " \"v\" \"" ++ [v] ++ "\")"