module U1
where
import Data.Char
import Data.List

type BoardField = (Int, Int, Char)
type Board = [BoardField]

validate :: String -> Bool
validate str = validateBoard $ parseBoard str

validateBoard :: Board -> Bool
validateBoard board = not(areThereDuplicateFiels board) &&
    if x == o
    then True
    else if x > o && x - 1 == o
         then True
         else if o > x && o - 1 == o
              then True
              else False
    where x = countFieldsX(board)
          o = countFieldsO(board)

areThereDuplicateFiels :: Board -> Bool
areThereDuplicateFiels board = length board /= length(nubBy areFieldsInSamePos board)

areFieldsInSamePos :: BoardField -> BoardField -> Bool
areFieldsInSamePos (x1, y1, _) (x2, y2, _) = if (x1 == x2 && y1 == y2) then True else False

countFieldsX :: Board -> Int
countFieldsX board = length(filter (\(_, _, x) -> x == 'x') board)

countFieldsO :: Board -> Int
countFieldsO board = length(filter (\(_, _, o) -> o == 'o') board)

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
parseBoardFields (x:xs) = [parseBoardField(x)] ++ parseBoardFields xs
