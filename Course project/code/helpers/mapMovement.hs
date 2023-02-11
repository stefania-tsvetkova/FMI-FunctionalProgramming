module Helpers.MapMovement where

import Data.Coordinates (Coordinates (x, y, Coordinates))
import Constants.Commands (
    moveLeftCommand, 
    moveRightCommand, 
    moveUpCommand, 
    moveDownCommand, 
    helpCommand, 
    exitCommand, 
    reviewGameCommand)
import Data.Player( 
    Player(Player, name, stats, history, canExchange, fightReinforcementsCount),
    position,
    addExchange,
    addHealth,
    removeHealth,
    addFightReinforcement,
    isAlive )
import Output.Errors (printIncorrectMovementInput)
import Output.Help (printMovementHelp)
import Helpers.Map (isPositionInMap, getElement, removeElement, getMoveText )
import Constants.Map (
    playerSymbol, 
    emptyCellSymbol, 
    exchangeSymbol, 
    fightReinforcementSymbol, 
    bonusLifeSymbol, 
    trapSymbol, 
    fightSymbol)
import Output.Moves (
    printOutsideOfMapMove, 
    printEmptyMove, 
    printPlayerEarnedExchange, 
    printPlayerEarnedBonusLife, 
    printTrapMove, 
    printPlayerEarnedFightReinforcement)
import Data.Stats (Stats(Stats, health, stamina))
import Helpers.EndOfGame (handleEndOfGame)
import Helpers.Fight (startFight)
import Input.Command (getCommand)
import Output.Player (printPlayerStats)
import Output.EndOfGame (reviewGame)

movePlayer :: Player -> [String] -> IO ()
movePlayer player gameMap =
    let
        updatedMap = removeElement gameMap (position player)
        correctInputs =
            [
                moveLeftCommand,
                moveUpCommand,
                moveRightCommand,
                moveDownCommand,
                helpCommand,
                exitCommand,
                reviewGameCommand
            ]
    in do
        if isAlive player then do
            printPlayerStats player

            input <- getCommand

            if input `notElem` correctInputs then do
                printIncorrectMovementInput
                movePlayer player updatedMap
            else
                handleInput input player updatedMap
        else
            handleEndOfGame player True updatedMap

handleInput :: String -> Player -> [String] -> IO ()
handleInput input player gameMap = do
    case () of
        ()
            | input == helpCommand -> do
                printMovementHelp
                movePlayer player gameMap
            | input == exitCommand ->
                handleEndOfGame player False gameMap
            | input == reviewGameCommand -> do
                reviewGame (history player)
                movePlayer player gameMap
            | otherwise -> do
                handleMapMovement input player gameMap

handleMapMovement :: String -> Player -> [String] -> IO ()
handleMapMovement input player gameMap =
    let 
        cellCoordinates = getMovementCoordinates input (position player)
    in 
        if isPositionInMap cellCoordinates gameMap then
            goToCell player gameMap cellCoordinates
        else do
            printOutsideOfMapMove
            movePlayer player gameMap

getMovementCoordinates :: String -> Coordinates -> Coordinates
getMovementCoordinates movementCommand initialCoordinates
    | movementCommand == moveLeftCommand = Coordinates row (column - 1)
    | movementCommand == moveRightCommand = Coordinates row (column + 1)
    | movementCommand == moveUpCommand = Coordinates (row - 1) column
    | movementCommand == moveDownCommand = Coordinates (row + 1) column
    | otherwise = error "Incorrect direction"
    where
        row = x initialCoordinates
        column = y initialCoordinates

goToCell :: Player -> [String] -> Coordinates -> IO ()
goToCell player gameMap cellCoordinates =
    let
        updatedPlayer = Player
            (name player)
            (stats player)
            cellCoordinates
            (canExchange player)
            (fightReinforcementsCount player)
            (getMoveText gameMap cellCoordinates : history player)

        cell = getElement gameMap cellCoordinates
    in
        case () of
            ()
                | cell == playerSymbol || cell == emptyCellSymbol -> playerInEmptyCell updatedPlayer gameMap
                | cell == exchangeSymbol -> playerInExchangeCell updatedPlayer gameMap
                | cell == fightReinforcementSymbol -> playerInFightReinforcementCell updatedPlayer gameMap
                | cell == bonusLifeSymbol -> playerInBonusLifeCell updatedPlayer gameMap
                | cell == trapSymbol -> playerInTrapCell updatedPlayer gameMap
                | cell == fightSymbol -> playerInFightCell updatedPlayer gameMap
                | otherwise -> playerOutsideOfMap player gameMap

playerInEmptyCell :: Player -> [String] -> IO ()
playerInEmptyCell player gameMap = do
    printEmptyMove
    movePlayer player gameMap

playerInExchangeCell :: Player -> [String] -> IO ()
playerInExchangeCell player gameMap = do
    printPlayerEarnedExchange
    movePlayer (addExchange player) gameMap

playerInFightReinforcementCell :: Player -> [String] -> IO ()
playerInFightReinforcementCell player gameMap = do
    printPlayerEarnedFightReinforcement
    movePlayer (addFightReinforcement player) gameMap

playerInBonusLifeCell :: Player -> [String] -> IO ()
playerInBonusLifeCell player gameMap = do
    printPlayerEarnedBonusLife
    movePlayer (addHealth player 1) gameMap

playerInTrapCell :: Player -> [String] -> IO ()
playerInTrapCell player gameMap = do
    printTrapMove
    movePlayer (removeHealth player 1) gameMap

playerOutsideOfMap :: Player -> [String] -> IO ()
playerOutsideOfMap player gameMap = do
    printOutsideOfMapMove
    movePlayer player gameMap

playerInFightCell :: Player -> [String] -> IO ()
playerInFightCell player gameMap = do
    updatedPlayer <- startFight player
    movePlayer updatedPlayer gameMap