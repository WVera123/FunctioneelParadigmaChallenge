import Data.Char (toLower, isDigit)
import Data.List (isInfixOf)

data BotState = Idle | WaitingForTracking

-- Higher-order function: any/all check if keyword exists in user input and takes function isInfixOf as an argument.
containsAny :: String -> [String] -> Bool -- Function takes a string, then a list of strings, and returns Bool.
containsAny input = any (`isInfixOf` (map toLower input)) --isInfixOf checks if input matches any keyword in the list. Input is set to lowercase to prevent case sensitivity issues.
containsAll :: String -> [String] -> Bool
containsAll input = all (`isInfixOf` (map toLower input))

-- Pattern matching on BotState and input
handleInput :: BotState -> String -> (String, BotState) -- Takes current state and user input, returns response and next state
handleInput Idle input
    | containsAll input ["waar", "pakket"] = ("Om uw pakketje te volgen heb ik uw trackingnummer nodig.", WaitingForTracking)
    | containsAny input ["hallo", "hi", "hey", "dag"] = ("Hallo! Hoe kan ik u helpen?", Idle)
    | containsAny input ["tracking", "nummer"] = ("U moet eerst vragen waar uw pakketje is voordat u een nummer opgeeft.", Idle)
    | input == "exit" || input == "stop" = ("Tot ziens!", Idle)
    | otherwise = ("Het spijt me, dat begrijp ik niet. Bedoelt u 'waar is mijn pakketje'?", Idle)

handleInput WaitingForTracking input
    | any isDigit input && containsAny input ["tracking", "nummer", "is", "hier"] = ("Bedankt! Uw pakketje is onderweg en zal binnen 2 werkdagen bij u zijn.", Idle)
    | otherwise = ("Ik heb een trackingnummer nodig om u te helpen (bijv. '123456').", WaitingForTracking)

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
    putStrLn "PostNL Chatbot gestart. Typ 'exit' om te stoppen."
    runBot Idle