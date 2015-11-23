module TTTcommon
where

type BoardField = (Int, Int, Char)
type Board = [BoardField]

scriptedGame :: Board
scriptedGame = [(0,0,'o'),(0,1,'x'),(0,2,'x'),(1,0,'x'),(1,1,'x'),(1,2,'o'),(2,0,'o'),(2,1,'o'),(2,2,'x')]

getNextMove :: Board -> Board
getNextMove currBoard = scriptedGameMock currBoard scriptedGame

scriptedGameMock :: Board -> Board -> Board
scriptedGameMock currBoard scriptedBoard = 
    if determineWhoseTurn currBoard == 'x'
    then filter(\(_, _, x) -> x == 'x') scriptedBoard !! (countFieldsX currBoard) : currBoard
    else filter(\(_, _, o) -> o == 'o') scriptedBoard !! (countFieldsO currBoard) : currBoard

determineWhoseTurn :: Board -> Char
determineWhoseTurn currBoard = 
    if countFieldsX(currBoard) == countFieldsO(currBoard)
    then 'x'
    else 'o'

countFieldsX :: Board -> Int
countFieldsX board = length(filter (\(_, _, x) -> x == 'x') board)

countFieldsO :: Board -> Int
countFieldsO board = length(filter (\(_, _, o) -> o == 'o') board)

gameStatus :: Board -> String
gameStatus currBoard
    | length currBoard == 9 = "Tie"
    | otherwise = "Ongoing"

-- Todo add win/lose check
