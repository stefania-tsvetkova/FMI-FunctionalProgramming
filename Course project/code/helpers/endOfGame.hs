module Helpers.EndOfGame where

import Data.Player (Player (history))
import Output.EndOfGame (printPlayerDied, printExitGame, reviewGame, printGoodbye)
import Control.Monad (when)
import Constants.Commands (reviewGameCommand)

handleEndOfGame :: Player -> Bool -> [String] -> IO ()
handleEndOfGame player isDead gameMap = do
    if isDead then
        printPlayerDied
    else
        printExitGame

    input <- getLine
    when (input == reviewGameCommand) (reviewGame (history player))

    printGoodbye