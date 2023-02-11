import Data.Player (Player(Player), stats, position)

import Output.Introduction (printIntroduction)
import Helpers.Map (getPlayerPosition, getElement)
import Helpers.MapMovement (movePlayer)
import Constants.InitialValues (initialStats, initialCanExchange, initialFightReinforcementsCount)
import Constants.Map (map1, map2)
import Input.Name (getName)
import System.Random
import System.IO.Unsafe

main :: IO ()
main = do
    name <- getName

    let gameMap = 
            if unsafePerformIO randomIO then
                map1
            else
                map2

    let playerPosition = getPlayerPosition gameMap

    let player = Player name initialStats playerPosition initialCanExchange initialFightReinforcementsCount []

    printIntroduction name
    
    movePlayer player gameMap