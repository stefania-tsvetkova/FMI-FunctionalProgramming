module Input.Name where

import Output.InputExplanation (printWaitingForName)
import Text.Read (Lexeme(String))
import Output.Errors (printEmptyNameError)

getName :: IO String
getName = do
    printWaitingForName
    name <- getLine
    putStrLn ""

    if name == "" then do
        printEmptyNameError
        getName
    else    
        return name