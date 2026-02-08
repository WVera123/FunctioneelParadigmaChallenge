import Data.Char (toLower, isDigit, isAlpha, isSpace)
import Data.List (isInfixOf)
import Control.Monad (unless)
import qualified Responses as R

-- Define states for a multi-step verification process
data BotState 
    = Idle 
    | WaitingForTracking 
    | WaitingForPostalCode 
    | WaitingForHouseNumber
    | WaitingForPackageStatus

-- Higher-order function: any/all check if keyword exists in user input and takes function isInfixOf as an argument.
containsAny :: String -> [String] -> Bool -- Function takes a string, then a list of strings, and returns Bool.
containsAny input = any (`isInfixOf` (map toLower input)) --isInfixOf checks if input matches any keyword in the list. Input is set to lowercase to prevent case sensitivity issues.
containsAll :: String -> [String] -> Bool
containsAll input = all (`isInfixOf` (map toLower input))

-- Higher-order function: filter/all
-- Pattern matching: on list of characters to validate postal code format (4 digits, 2 letters)
isValidPostal :: String -> Bool
isValidPostal input = case filter (not . isSpace) input of -- Filter: keeps only non-space characters from input. Case: tries to match input against pattern.
    [d1,d2,d3,d4,l1,l2] -> all isDigit [d1,d2,d3,d4] && all isAlpha [l1,l2] -- Binds first 4 characters to d1-d4 and checks if they are digits, then binds last 2 characters to l1-l2 and checks if they are letters.
    _                   -> False -- Any input that doesn't match the pattern will return False.

isValidTracking :: String -> Bool
isValidTracking input = case filter (not . isSpace) input of
    [d1,d2,d3,d4,d5,d6] -> all isDigit [d1,d2,d3,d4,d5,d6]
    _                   -> False

-- Pattern matching on BotState and input
handleInput :: BotState -> String -> (String, BotState) -- Takes current state and user input, returns response and next state
handleInput state input -- Binds parameters state and input
    | containsAny input ["help", "hulp"] = (provideHelp state, state) -- If user input is "help", provide contextual help based on current state and remain in the same state	
    | containsAny input ["exit", "stop"] = (R.msgExit, Idle) -- Returns exit message and sets state to Idle

-- IDLE state
handleInput Idle input -- Only checks input for these keywords when in Idle state
    | containsAny input ["waar", "wanneer", "pakket", "trackingnummer"] = (R.msgAskTracking, WaitingForTracking)
    | containsAny input ["hallo", "hi", "dag", "hoi"]      = (R.msgGreeting, Idle)
    | otherwise                                     = (R.msgUnknown, Idle)

-- TRACKING state
handleInput WaitingForTracking input
    | isValidTracking input = (R.msgFinalSuccess, WaitingForPackageStatus)
    | containsAny input ["waar", "vinden"] = (R.helpTracking, WaitingForTracking)
    | containsAny input ["niet", "geen"] = (R.msgAskPostalCode, WaitingForPostalCode)
    | otherwise         = (R.msgAskTracking, WaitingForTracking)

-- POSTAL CODE state
handleInput WaitingForPostalCode input
    | isValidPostal input = (R.msgAskHouseNumber, WaitingForHouseNumber)
    | otherwise           = (R.msgInvalidPostal, WaitingForPostalCode)

-- HOUSE NUMBER state
handleInput WaitingForHouseNumber input
    | any isDigit input = (R.msgFinalSuccess, WaitingForPackageStatus)
    | otherwise         = (R.msgInvalidHouse, WaitingForHouseNumber)

provideHelp :: BotState -> String
provideHelp state = case state of --Based on BotState, return corresponding help message
    Idle                 -> R.helpIdle
    WaitingForTracking   -> R.helpTracking
    WaitingForPostalCode -> R.helpPostal
    WaitingForHouseNumber -> R.helpHouse

-- Recursion: The function calls itself to implement loop: runBot nextState.
runBot :: BotState -> IO ()
runBot state = do
    input <- getLine -- Get user input
    let (response, nextState) = handleInput state input
    putStrLn $ "PostNL: " ++ response
    -- Terminate recursion if user exits, otherwise continue with nextState
    unless (containsAny input ["exit", "stop"]) $
        runBot nextState

main :: IO ()
main = do
    putStrLn R.msgWelcome
    runBot Idle