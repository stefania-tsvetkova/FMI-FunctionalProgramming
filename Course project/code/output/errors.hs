module Output.Errors where
    
import Output.Help (printHelpCommand)
import Output.Fight (printFightHelp)

printIncorrectMovementInput :: IO ()
printIncorrectMovementInput = do
    putStrLn "This is not a correct command. Please try again."

    printHelpCommand

    putStrLn ""

printIncorrectFightInput :: Bool -> Bool -> IO ()
printIncorrectFightInput playerHasReinforcement playerCanExchange = do
    putStrLn "This is not a correct command. Please try again."

    printFightHelp playerHasReinforcement playerCanExchange

    putStrLn ""

printEmptyNameError :: IO ()
printEmptyNameError =
    putStrLn "Have you made a mistake? Your name cannot be nothing..\n"