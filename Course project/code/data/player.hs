module Data.Player where

import Data.Stats (Stats (Stats, stamina, health))
import Data.Coordinates (Coordinates)

data Player = Player {
    name :: String,
    stats :: Stats,
    position :: Coordinates,
    canExchange :: Bool,
    fightReinforcementsCount :: Int,
    history :: [String]
}

isAlive :: Player -> Bool
isAlive player =
    playerHealth > 0 && palyerStamina > 0
    where
        playerHealth = health (stats player)
        palyerStamina = stamina (stats player)

exchangeStats :: Player -> Stats -> Player
exchangeStats player statsToExchange =
    Player
        (name player)
        (Stats newHealth newStamina)
        (position player)
        (canExchange player)
        (fightReinforcementsCount player)
        (history player)
        where
            newHealth = health (stats player) - health statsToExchange
            newStamina = stamina (stats player) + stamina statsToExchange

addStats :: Player -> Stats -> Player
addStats player statsToAdd =
    Player
        (name player)
        (Stats newHealth newStamina)
        (position player)
        (canExchange player)
        (fightReinforcementsCount player)
        (history player)
        where
            newHealth = health (stats player) + health statsToAdd
            newStamina = stamina (stats player) + stamina statsToAdd

removeStats :: Player -> Stats -> Player
removeStats player statsToRemove =
    addStats player (Stats negativeHealth negativeStamina)
    where 
        negativeHealth = negate (health statsToRemove)
        negativeStamina = negate (stamina statsToRemove)

addHealth :: Player -> Int -> Player
addHealth player healthToAdd =
    addStats player (Stats healthToAdd 0)

removeHealth :: Player -> Int -> Player
removeHealth player healthToRemove =
    addHealth player (negate healthToRemove)

addExchange :: Player -> Player
addExchange player =
    Player
        (name player)
        (stats player)
        (position player)
        True
        (fightReinforcementsCount player)
        (history player)

addFightReinforcement :: Player -> Player
addFightReinforcement player =
    updateFightReinforcements player 1

removeFightReinforcement :: Player -> Player
removeFightReinforcement player =
    updateFightReinforcements player (negate 1)

updateFightReinforcements :: Player -> Int -> Player
updateFightReinforcements player fightReinforcementsToAdd =
    Player
        (name player)
        (stats player)
        (position player)
        (canExchange player)
        (playerFightReinforcementsCount + fightReinforcementsToAdd)
        (history player)
        where
            playerFightReinforcementsCount = fightReinforcementsCount player