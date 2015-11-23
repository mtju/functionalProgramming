{-# LANGUAGE OverloadedStrings #-}
module Dmode (dMode)
where
import TTTcommon
import Network.Wreq
import Control.Lens
import SxprList
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CLz

dMode :: String -> IO ()
dMode gameName = do
    defendLoop gameName

defendLoop :: String -> IO ()
defendLoop gameName = do
    oppMove <- getOpponentMove gameName
    if (gameStatus $ parseBoard oppMove) /= "Ongoing"
        then do putStrLn $ gameStatus $ parseBoard oppMove
        else do postMove gameName $ getNextMove $ parseBoard oppMove
                if (gameStatus $ getNextMove $ parseBoard oppMove) /= "Ongoing"
                    then do putStrLn $ gameStatus $ getNextMove $ parseBoard oppMove
                    else do defendLoop gameName

postMove :: String -> Board -> IO ()
postMove gameName board = do
    let opts = defaults & header "Content-Type" .~ ["application/s-expr+list"] -- change me
    let postData = C.pack (exportBoard board)
    postWith opts ("http://tictactoe.homedir.eu/game/" ++ gameName ++ "/player/2") postData
    putStrLn "Move posted."

getOpponentMove :: String -> IO String
getOpponentMove gameName = do
    putStrLn "Waiting for opponent."
    let opts = defaults & header "Accept" .~ ["application/s-expr+list"] -- change me
    r <- getWith opts ("http://tictactoe.homedir.eu/game/" ++ gameName ++ "/player/2")
    let returnString = CLz.unpack (r ^. responseBody)
    return returnString