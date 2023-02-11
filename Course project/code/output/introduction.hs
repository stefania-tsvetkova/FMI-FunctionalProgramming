module Output.Introduction where

import Data.Stats (Stats(health, stamina))
import Output.Help (printMovementHelp, printHelpCommand, printExitGameCommand, printReviewGameCommand)
import Constants.InitialValues (initialStats)

printIntroduction :: String -> IO ()
printIntroduction name = do
    putStrLn $
        "Hello, " ++ name ++ "! Nice to meet you!\n"
        ++ "You start the game with " 
        ++ show (health initialStats) ++ " health and " 
        ++ show (stamina initialStats) ++ " stamina.\n"
        ++ "You can win/lose health and stamina when you participate in fights, find bonuses or enter traps.\n"
        ++ "Your goal is not to die :)\n"

    printMovementHelp 

    printHelpCommand
    printReviewGameCommand
    printExitGameCommand

    putStrLn ""