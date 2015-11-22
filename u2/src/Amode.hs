module Amode (aMode)
where
import TTTcommon
import Network.Wreq
import Control.Lens
import SxprList
import qualified Data.ByteString.Char8 as C

aMode :: String -> IO ()
aMode gameName = do
    postMove gameName $ getNextMove []

--postBoard :: Board -> 

--Game status

--NextMove
postMove :: String -> Board -> IO ()
postMove gameName board = do
    let opts = defaults & header "Content-Type" .~ ["application/s-expr+list"]
    let firstData = C.pack (exportBoard board)
    postWith opts ("http://tictactoe.homedir.eu/game/" ++ gameName ++ "/player/1") firstData
    putStrLn "Move posted."

--getBoard :: String ->