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
    currBoardStr <- getOpponentMove gameName
    putStrLn currBoardStr
    --putStrLn (getOpponentMove gameName)
    --postMove gameName $ getNextMove $ getOpponentMove gameName


postMove :: String -> Board -> IO ()
postMove gameName board = do
    let opts = defaults & header "Content-Type" .~ ["application/s-expr+list"] -- change me
    let postData = C.pack (exportBoard board)
    postWith opts ("http://tictactoe.homedir.eu/game/" ++ gameName ++ "/player/2") postData
    putStrLn "Move posted."

getOpponentMove :: String -> IO String
getOpponentMove gameName = do
    let opts = defaults & header "Accept" .~ ["application/s-expr+list"] -- change me
    r <- getWith opts ("http://tictactoe.homedir.eu/game/" ++ gameName ++ "/player/2")
    --putStrLn $ CLz.unpack (r ^. responseBody)
    --testString <- show (r ^. responseBody)
    let testString = show (r ^. responseBody)
    return testString
    --return "(l (m \"x\" 0 \"y\" 1 \"v\" \"x\"))"