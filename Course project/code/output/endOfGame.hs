module Output.EndOfGame where

import Constants.Commands (reviewGameCommand)
import Output.Player (printHistory)
import Data.Coordinates (Coordinates)

printPlayerDied :: IO ()
printPlayerDied =
    putStrLn $
        "Oh no, you died :(\n"
        ++ reviewMovesText

printExitGame :: IO ()
printExitGame =
    putStrLn $
        "Game stopped.\n"
        ++ reviewMovesText

reviewMovesText :: String
reviewMovesText = 
    "If you want to review your moves, type '" ++ reviewGameCommand ++ "'. "
    ++ "Any other command will close the game. \n"

reviewGame :: [String] -> IO ()
reviewGame history = do
    putStrLn "Here is this game's timeline:" 
    printHistory (reverse history)
    putStrLn ""

printGoodbye :: IO ()
printGoodbye =
    putStrLn $
        "Thank you for playing the game!\n"
        ++ "Byeee :)"