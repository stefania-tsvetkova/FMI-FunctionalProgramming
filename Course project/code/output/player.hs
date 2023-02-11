module Output.Player where

import Data.Stats (Stats(health, stamina))
import Data.Player (Player(stats))
import Data.Coordinates (Coordinates)
import Helpers.Map (getElement)
import Constants.Map (
    exchangeSymbol, 
    fightReinforcementSymbol, 
    bonusLifeSymbol, 
    trapSymbol, 
    fightSymbol)

printPlayerStats :: Player -> IO ()
printPlayerStats player =
    putStrLn $
        "You have " ++ show (health (stats player)) ++ " health "
        ++ "and " ++ show (stamina (stats player)) ++ " stamina.\n"

printHistory :: [String]  -> IO ()
printHistory [] = putStr ""
printHistory (move:nextMoves) = do
    putStrLn move
    printHistory nextMoves