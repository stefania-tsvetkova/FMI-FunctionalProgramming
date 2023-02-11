module Output.InputExplanation where

printWaitingForCommand :: IO ()
printWaitingForCommand = 
    putStrLn "Waiting for your command.."

printWaitingForName :: IO ()
printWaitingForName = 
    putStrLn "What is your name?"