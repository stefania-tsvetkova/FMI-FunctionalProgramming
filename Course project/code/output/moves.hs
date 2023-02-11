module Output.Moves where

printEmptyMove :: IO ()
printEmptyMove = 
    putStrLn "There is noone and nothing near you.\n"

printOutsideOfMapMove :: IO ()
printOutsideOfMapMove = 
    putStrLn "The woods are too dense, you can't go there. Please choose other direction.\n"

printPlayerEarnedExchange :: IO ()
printPlayerEarnedExchange =
    putStrLn $
        "You found a ginny in a lamp and wished to be able to exchange "
        ++ "health for stamina and vice versa in duels.\n"

printPlayerEarnedBonusLife :: IO ()
printPlayerEarnedBonusLife =
    putStrLn "Yay! You won 1 bonus life.\n"

printPlayerEarnedFightReinforcement :: IO ()
printPlayerEarnedFightReinforcement =
    putStrLn $
        "You found a magic potion drink and drank it. "
        ++ "Now you have 1 new reinforcement, that you could use if needed in some duel."

    
printTrapMove :: IO ()
printTrapMove = 
    putStrLn "You entered a trap and lost 1 life.\n"