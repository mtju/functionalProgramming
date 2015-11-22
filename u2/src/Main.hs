module Main where

import Amode
import Dmode

main :: IO ()
main = do
    putStrLn "Game name: "  
    gameName <- getLine
    putStrLn "Game mode: A - attack, D - defend?"
    mode <- getLine
    case mode of
        "A" -> aMode gameName
        "D" -> dMode gameName
        _ -> putStrLn "Bad choice."

