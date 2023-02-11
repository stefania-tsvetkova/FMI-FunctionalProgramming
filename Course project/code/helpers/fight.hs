module Helpers.Fight where
    
import Output.Fight (
    printFight,
    printWonFight,
    printLostFight,
    printStartFight,
    printExchange,
    printErrorExchange)
import Data.Player (
    Player(fightReinforcementsCount, canExchange, stats),
    removeFightReinforcement,
    removeStats,
    addStats,
    exchangeStats)
import Constants.Commands (fightCommand, fightReinforcementCommand, exchangeCommand)
import Output.Errors (printIncorrectFightInput)
import Constants.Stats (fightWeight, exchangeRate)
import Data.Stats (Stats(stamina, health))
import Input.Command (getCommand)

startFight :: Player -> IO Player
startFight player =
    let
        playerHasReinforcement = fightReinforcementsCount player > 0
        playerCanExchange = canExchange player
    in do
        printFight playerHasReinforcement playerCanExchange

        fight player

fight :: Player -> IO Player
fight player =
    let
        playerHasReinforcement = fightReinforcementsCount player > 0
        playerCanExchange = canExchange player
    in do
        input <- getCommand

        case () of
            ()
                | input == fightCommand ->
                    handleFight player
                | playerHasReinforcement && input == fightReinforcementCommand ->
                    handleFightReinforcement player
                | playerCanExchange && (input == exchangeCommand) ->
                    handleExchange player
                | otherwise -> do
                    printIncorrectFightInput playerHasReinforcement playerCanExchange
                    fight player

handleFight :: Player -> IO Player
handleFight player =
    let
        playerStamina = stamina (stats player)
        opponentStamina = stamina fightWeight
    in do
        printStartFight

        if playerStamina > opponentStamina then do
            printWonFight
            return (addStats player fightWeight)
        else do
            printLostFight
            return (removeStats player fightWeight)

handleFightReinforcement :: Player -> IO Player
handleFightReinforcement player = do
    printWonFight
    return (removeFightReinforcement (addStats player fightWeight))

handleExchange :: Player -> IO Player
handleExchange player =
    let
        playerHealth = health (stats player)
    in do
        if playerHealth > health exchangeRate then do
            printExchange
            handleFight (exchangeStats player exchangeRate)
        else do
            printErrorExchange
            handleFight player
