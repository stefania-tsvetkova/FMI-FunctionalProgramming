module Constants.Map where

map1 :: [String]
map1 =
    [
        "  * ^  ",
        "-#---* ",
        "+ @   ?",
        "  #  ? "
    ]

map2 :: [String]
map2 =
    [
        "  *  ",
        " #@|^",
        "  -  ",
        " +?  "
    ]

mapRows :: [String] -> Int
mapRows = length

mapColumns :: [String] -> Int
mapColumns gameMap = length (gameMap !! 1)

playerSymbol :: Char
playerSymbol = '@'

emptyCellSymbol :: Char
emptyCellSymbol = ' '

exchangeSymbol :: Char
exchangeSymbol = '^'

fightReinforcementSymbol :: Char
fightReinforcementSymbol = '*'

bonusLifeSymbol :: Char
bonusLifeSymbol = '+'

trapSymbol :: Char
trapSymbol = '#'

fightSymbol :: Char
fightSymbol = '?'