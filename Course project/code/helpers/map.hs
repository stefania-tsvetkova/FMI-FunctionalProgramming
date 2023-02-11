module Helpers.Map where

import Constants.Map (
    playerSymbol, 
    mapRows, 
    mapColumns, 
    emptyCellSymbol, 
    exchangeSymbol, 
    fightReinforcementSymbol, 
    bonusLifeSymbol, 
    trapSymbol, 
    fightSymbol)
import Data.Coordinates (Coordinates (Coordinates), x, y)

getElement :: [String] -> Coordinates -> Char
getElement gameMap coordinates =
    (gameMap !! row) !! column
    where
        row = x coordinates
        column = y coordinates

isPositionInMap :: Coordinates -> [String] -> Bool
isPositionInMap coordinates gameMap =
    row >= 0 && row < rowsCount && column >= 0 && column < columnsCount
    where
        row = x coordinates
        column = y coordinates
        rowsCount = mapRows gameMap
        columnsCount = mapColumns gameMap

getPlayerPosition :: [String] -> Coordinates
getPlayerPosition =
    getPlayerPosition 0
    where
        getPlayerPosition :: Int -> [String] -> Coordinates
        getPlayerPosition _ [] = error "No player found"
        getPlayerPosition i (row:remainingRows) =
            let 
                j = getPlayerColumn row
            in
                if j >= 0 then
                    Coordinates i j
                else
                    getPlayerPosition (i + 1) remainingRows
            where
                getPlayerColumn :: String -> Int
                getPlayerColumn [] = -1
                getPlayerColumn (head:tail)
                    | head == playerSymbol = 0
                    | tailResult == -1 = -1
                    | otherwise = 1 + tailResult
                    where
                        tailResult = getPlayerColumn tail

removeElement :: [String] -> Coordinates -> [String]
removeElement [] _ = []
removeElement (row:remainingRows) coordinates =
    if x coordinates == 0 then
        removeElementFromRow row (y coordinates) : remainingRows
    else
        row : removeElement remainingRows (Coordinates (x coordinates - 1) (y coordinates))
    where
        removeElementFromRow :: String -> Int -> String
        removeElementFromRow [] _ = []
        removeElementFromRow (_:remainingElements) 0 = emptyCellSymbol : remainingElements
        removeElementFromRow (element:remainingElements) column =
            element : removeElementFromRow remainingElements (column - 1)

getMoveText :: [String] -> Coordinates -> String
getMoveText gameMap coordinates
    | cell == exchangeSymbol = "- Earned the possibility to do exchanges"
    | cell == fightReinforcementSymbol = "- Earned 1 fight reinforcement"
    | cell == bonusLifeSymbol = "- Earned 1 bonus life"
    | cell == trapSymbol = "- Entered a trap"
    | cell == fightSymbol = "- Participated in a duel"
    | otherwise = "- Travelled a little bit"
    where
        cell = getElement gameMap coordinates