{-# LANGUAGE OverloadedStrings #-}
module Amode (aMode)
where
import TTTcommon
import Network.Wreq
import Control.Lens
import SxprList
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CLz

aMode :: String -> IO ()
aMode gameName = do
    attackLoop gameName []

attackLoop :: String -> Board -> IO ()
attackLoop gameName currBoard = do
    postMove gameName $ getNextMove currBoard
    oppMove <- getOpponentMove gameName
    attackLoop gameName $ parseBoard oppMove

postMove :: String -> Board -> IO ()
postMove gameName board = do
    let opts = defaults & header "Content-Type" .~ ["application/s-expr+list"]
    let postData = C.pack (exportBoard board)
    postWith opts ("http://tictactoe.homedir.eu/game/" ++ gameName ++ "/player/1") postData
    putStrLn "Move posted."

getOpponentMove :: String -> IO String
getOpponentMove gameName = do
    putStrLn "Waiting for opponent."
    let opts = defaults & header "Accept" .~ ["application/s-expr+list"] -- change me
    r <- getWith opts ("http://tictactoe.homedir.eu/game/" ++ gameName ++ "/player/1")
    let returnString = CLz.unpack (r ^. responseBody)
    return returnString