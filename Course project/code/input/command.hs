module Input.Command where

import Output.InputExplanation (printWaitingForCommand)

getCommand :: IO String
getCommand = do
    printWaitingForCommand
    command <- getLine
    putStrLn ""
    
    return command