module Output.Fight where
    
import Data.Stats (Stats(health, stamina))
import Constants.Stats (fightWeight, exchangeRate)
import Constants.Commands (fightCommand, fightReinforcementCommand, exchangeCommand)

printFight :: Bool -> Bool -> IO ()
printFight playerHasReinforcement playerCanExchange = do
    putStrLn "You met a knight, who challenges you to a duel. Could you win this fight?"

    putStrLn $
        "The knight has " ++ show (stamina fightWeight) ++ " stamina. "
        ++ "It you win the duel, you will earn "
        ++ show (health fightWeight) ++ " health and " 
        ++ show (stamina fightWeight) ++ " stamina. "
        ++ "Otherwise you will lose them.\n"
    
    printFightHelp playerHasReinforcement playerCanExchange
    
    putStrLn ""

printFightHelp :: Bool -> Bool -> IO ()
printFightHelp playerHasReinforcement playerCanExchange = do
    putStrLn $
        "Possible comands:\n"
        ++ "> " ++ fightCommand

    if playerHasReinforcement then
        putStrLn $
            "> " ++ fightReinforcementCommand ++ "\n"
            ++ "  You can use the reinforcement only once. If you use it now, you will win the fight."
    else
        putStr ""

    if playerCanExchange then
        putStrLn $
            "> " ++ exchangeCommand ++ "\n"
            ++ "  The exchange creates " ++ show (health exchangeRate) ++ " health "
            ++ "from " ++ show (stamina exchangeRate) ++ " stamina."
    else
        putStr ""

printStartFight :: IO ()
printStartFight = 
    putStrLn "The duel started. Good luck!\n"

printWonFight :: IO ()
printWonFight = 
    putStrLn "Yay, you won this duel!\n"

printLostFight :: IO ()
printLostFight = 
    putStrLn "The knight was a little bit too strong for you, you lost the duel.\n"

printExchange :: IO ()
printExchange = 
    putStrLn $
        show (health exchangeRate) ++ " health is being transformed to "
        ++ show (stamina exchangeRate) ++ " stamina.\n"

printErrorExchange :: IO ()
printErrorExchange =
    putStrLn "Ops, you don't have enough health to do the exchange.\n"