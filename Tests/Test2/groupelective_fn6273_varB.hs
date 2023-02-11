-- Problem 1

addToDictionary :: String -> Int -> [(String, Int)] -> [(String, Int)]
addToDictionary key value [] = [(key, value)]
addToDictionary key value ((currKey, currValue):tail) =
    if key == currKey then
        (key, value + currValue):tail
    else
        (currKey, currValue) : addToDictionary key value tail

getPoints :: [(String, String, Int, Int)] -> [(String, Int)] -> [(String, Int)]
getPoints [] points = points
getPoints ((name1, name2, goals1, goals2):tail) points
    | goals1 > goals2 = getPoints tail (addToDictionary name1 3 points)
    | goals1 < goals2 = getPoints tail (addToDictionary name2 3 points)
    | otherwise = getPoints tail (addToDictionary name1 1 (addToDictionary name2 1 points))
        
getGoalsDiff :: [(String, String, Int, Int)] -> [(String, Int)] -> [(String, Int)]
getGoalsDiff [] goalsDiff = goalsDiff
getGoalsDiff ((name1, name2, goals1, goals2):tail) goalsDiff =
    if goals1 == goals2 then
        getGoalsDiff tail goalsDiff
    else
        getGoalsDiff tail (calculateGoals (name1, name2, goals1, goals2) goalsDiff)
        where
            calculateGoals :: (String, String, Int, Int) -> [(String, Int)] -> [(String, Int)]
            calculateGoals (name1, name2, goals1, goals2) goalsDiff =
                let
                    goals1to2 = addToDictionary name1 goals1 (addToDictionary name2 (-goals1) goalsDiff)
                    goals2to1 = addToDictionary name1 (-goals2) (addToDictionary name2 goals2 goals1to2)
                in goals2to1


getMaxPoints :: [(String, Int)] -> (String, Int) -> Int
getMaxPoints [] (_, points) = points
getMaxPoints ((name, points):tail) (currMaxName, currMaxPoints) =
    if points > currMaxPoints then
        getMaxPoints tail (name, points)
    else
        getMaxPoints tail (currMaxName, currMaxPoints)

getMinGoalsDiff :: [(String, Int)] -> (String, Int) -> String
getMinGoalsDiff [] (minName, _) = minName
getMinGoalsDiff ((name, goalsDiff):tail) (currMinName, currMinGoalsDiff) =
    if goalsDiff < currMinGoalsDiff then
        getMinGoalsDiff tail (name, goalsDiff)
    else
        getMinGoalsDiff tail (currMinName, currMinGoalsDiff)

maxPointsMinGoal :: [(String, String, Int, Int)] -> String
maxPointsMinGoal [] = ""
maxPointsMinGoal tournament =
    let
        points = getPoints tournament []
        goalsDiff = getGoalsDiff tournament []
        maxPoints = getMaxPoints points (head points)
        teamsWithMaxPoints = filter (\(name, currPoints) -> currPoints == maxPoints) points
        teamsGoalsDiff = filter (\(name, giff) -> any (\(team,_) -> team == name) teamsWithMaxPoints) goalsDiff
    in getMinGoalsDiff teamsGoalsDiff (head teamsGoalsDiff)

wins :: String -> String -> [(String, String, Int, Int)] -> Bool
wins _ _ [] = False
wins winner loser ((name1, name2, goals1, goals2):tail)
    | winner == name1 && loser == name2 = goals1 > goals2 || wins winner loser tail
    | winner == name2 && loser == name1 = goals2 > goals1 || wins winner loser tail
    | otherwise = wins winner loser tail

winsEveryTime :: String -> String -> [(String, String, Int, Int)] -> Bool
winsEveryTime _ _ [] = False
winsEveryTime winner loser ((name1, name2, goals1, goals2):tail)
    | winner == name1 && loser == name2 = goals1 > goals2 && wins winner loser tail
    | winner == name2 && loser == name1 = goals2 > goals1 && wins winner loser tail
    | otherwise = wins winner loser tail

addToList :: String -> [String] -> [String]
addToList value [] = [value]
addToList value (head:tail) =
    if value == head then
        head:tail
    else
        head : addToList value tail

allTeams :: [(String, String, Int, Int)] -> [String] -> [String]
allTeams [] teams = teams
allTeams ((name1, name2, _, _):tail) teams =
    addTeam name1 (addTeam name2 teams)
    where
        addTeam :: String -> [String] -> [String]
        addTeam name teams =
            if name `elem` teams then
                allTeams tail teams
            else
                allTeams tail (addToList name teams)

exceeds :: String -> String -> [(String, String, Int, Int)] -> [String] -> Bool
exceeds _ _ _ [] = False
exceeds winner loser tournaments (middleTeam:tail) =
    wins winner loser tournaments ||
    (winsEveryTime winner middleTeam tournaments && exceeds middleTeam loser tournaments tail) ||
    exceeds winner loser tournaments tail

exceedsSelf :: String -> [(String, String, Int, Int)] -> [String] -> Bool
exceedsSelf team = exceeds team team

exceedSelf tournament =
    let
        teams = allTeams tournament []
    in helper teams tournament teams
    where
        helper :: [String] -> [(String, String, Int, Int)] -> [String] -> [String]
        helper [] _ _ = []
        helper (team:tail) tournaments teams =
            if exceedsSelf team tournaments teams then
                team : helper tail tournaments teams
            else
                helper tail tournaments teams

-- Problem 2
-- tree = [list of nodes]
-- node with index i
-- left = i * 2 + 1
-- right = i * 2 + 2

trace :: [Int -> Bool] -> Int  -> [Int]
trace tree n =
    trace tree 0 n
    where
        trace :: [Int -> Bool] -> Int -> Int  -> [Int]
        trace tree i n = 
            if i >= length tree then
                []
            else
                i : (if predicate n then trace tree (i * 2 + 2) n else trace tree (i * 2 + 1) n)
            where
                predicate = tree !! i

sameTrace :: [Int -> Bool] -> [Int] -> Bool
sameTrace tree nums = 
    sameTrace tree nums 0 0
    where
        sameTrace :: [Int -> Bool] -> [Int] -> Int -> Int -> Bool
        sameTrace tree nums i j
            | i >= length nums = False
            | j >= length nums = sameTrace tree nums (i + 1) 0
            | i == j = sameTrace tree nums i (j + 1)
            | otherwise = trace tree (nums !! i) == trace tree (nums !! j) || sameTrace tree nums i (j + 1)