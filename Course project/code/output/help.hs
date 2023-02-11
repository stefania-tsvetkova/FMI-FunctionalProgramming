module Output.Help where
    
import Constants.Commands (
    moveLeftCommand,
    moveRightCommand,
    moveUpCommand,
    moveDownCommand, 
    exitCommand, 
    helpCommand, 
    reviewGameCommand)

printMovementHelp :: IO ()
printMovementHelp = 
    putStrLn $
        "You can move in the map typing one of these commands:\n"
        ++ "> " ++ moveLeftCommand ++ "\n"
        ++ "> " ++ moveRightCommand ++ "\n"
        ++ "> " ++ moveUpCommand ++ "\n"
        ++ "> " ++ moveDownCommand ++ "\n"

printHelpCommand :: IO ()
printHelpCommand =
    putStrLn $ "If you need help, you can type '" ++ helpCommand ++ "'"

printReviewGameCommand :: IO ()
printReviewGameCommand =
    putStrLn $ 
        "If you want to review the game until now, "
        ++ "type '" ++ reviewGameCommand ++ "'"

printExitGameCommand :: IO ()
printExitGameCommand =
    putStrLn $ "If you want to stop the game, you can type '" ++ exitCommand ++ "'"