import Data.Char (toLower, isDigit)
import Data.List (isInfixOf)
import qualified Responses as R

data BotState = Idle | WaitingForTracking

-- Higher-order function: any/all check if keyword exists in user input and takes function isInfixOf as an argument.
containsAny :: String -> [String] -> Bool -- Function takes a string, then a list of strings, and returns Bool.
containsAny input = any (`isInfixOf` (map toLower input)) --isInfixOf checks if input matches any keyword in the list. Input is set to lowercase to prevent case sensitivity issues.
containsAll :: String -> [String] -> Bool
containsAll input = all (`isInfixOf` (map toLower input))

-- Pattern matching on BotState and input
handleInput :: BotState -> String -> (String, BotState)
handleInput Idle input    
    | containsAll norm ["waar", "pakket"] = (R.msgAskTracking, WaitingForTracking)
    | containsAny norm ["hallo", "hi", "hey", "dag"] = (R.msgGreeting, Idle)
    | containsAny norm ["tracking", "nummer"] = (R.msgRejectTracking, Idle)
    | norm `elem` ["exit", "stop"] = (R.msgExit, Idle)
    | otherwise = (R.msgUnknown, Idle)
    where norm = map toLower input

handleInput WaitingForTracking input
    | any isDigit input = (R.msgTrackingSuccess, Idle)
    | otherwise = (R.msgTrackingRetry, WaitingForTracking)
    where norm = map toLower input
-- Recursion: The function calls itself to implement loop: runBot nextState.
runBot :: BotState -> IO ()
runBot state = do
    input <- getLine
    let (response, nextState) = handleInput state input
    putStrLn $ "PostNL: " ++ response
    if containsAny input ["exit", "stop"]
        then return ()
        else runBot nextState

main :: IO ()
main = do
    putStrLn R.msgWelcome
    runBot Idle