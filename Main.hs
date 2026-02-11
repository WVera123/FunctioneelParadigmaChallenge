import Data.Char (toLower, isDigit, isAlpha, isSpace)
import Data.List (isInfixOf)
import Control.Monad (unless)
import Control.Concurrent (threadDelay)
import System.Random 
import Text.Printf (printf)
import qualified Responses as R

data BotState 
    = Idle 
    | WaitingForTracking 
    | WaitingForPostalCode 
    | WaitingForHouseNumber
    | WaitingForPackageStatus
    | WaitingForKgPackage
    | WaitingForDestinationPackage Double            -- carries weight
    | WaitingForShippingCalculation Double String    -- carries weight and destination

-- Higher-order function: any/all check if keyword exists in user input and takes function isInfixOf as an argument.
containsAny :: String -> [String] -> Bool -- Function takes a string, then a list of strings, and returns Bool.
containsAny input = any (`isInfixOf` (map toLower input)) --isInfixOf checks if input matches any keyword in the list. Input is set to lowercase to prevent case sensitivity issues.
containsAll :: String -> [String] -> Bool
containsAll input = all (`isInfixOf` (map toLower input))

clearWhiteSpace :: String -> String
clearWhiteSpace = filter (not . isSpace)

-- Higher-order function: filter/all
-- Pattern matching: on list of characters to validate postal code format (4 digits, 2 letters)
isValidDestination :: String -> Bool
isValidDestination input = containsAny input ["binnenlands", "binnen", "buitenlands", "buiten"]

isValidPostal :: String -> Bool
isValidPostal input = case clearWhiteSpace input of -- Filter: keeps only non-space characters from input. Case: tries to match input against pattern.
    [d1,d2,d3,d4,l1,l2] -> all isDigit [d1,d2,d3,d4] && all isAlpha [l1,l2] -- Binds first 4 characters to d1-d4 and checks if they are digits, then binds last 2 characters to l1-l2 and checks if they are letters.
    _                   -> False -- Any input that doesn't match the pattern will return False.

isValidTracking :: String -> Bool
isValidTracking input = case clearWhiteSpace input of
    [d1,d2,d3,d4,d5,d6] -> all isDigit [d1,d2,d3,d4,d5,d6]
    _                   -> False

isValidKg :: String -> Bool
isValidKg input =
    let cleaned = clearWhiteSpace input
    in not (null cleaned) --Ensure input isn't empty after clean
       && all (\c -> isDigit c || c == '.') cleaned --Every remaining char is a digit or '.'.
       && case reads cleaned :: [(Double, String)] of -- Reads attempts to parse a Double from input and returns a list of successful parses.
            [(weight, rest)] -> null rest -- Valid if entire string can be parsed as Double, so 'kg' behind it isn't allowed.
            _           -> False

-- Parse kg; caller should ensure input is valid with isValidKg
parseKg :: String -> Double
parseKg input =
    case reads (clearWhiteSpace input) :: [(Double, String)] of
         [(weight, _)] -> weight
         _        -> 0.0

getChanceOutcome :: Int -> String
getChanceOutcome n
    | n <= 80   = R.msgFinalSuccess -- 80% Success
    | n <= 95   = R.msgFinalDelay   -- 15% Delay
    | otherwise = R.msgFinalProblem -- 5% Missing/Problem

-- Calculate shipping cost based on weight and destination.
-- For "binnenlands" use weight * 0.5, for "buitenlands" use weight * 0.75.
calculateShipping :: Double -> String -> Double
calculateShipping weight dest
    | containsAny dest ["binnenlands", "binnen"] = weight * 0.5
    | containsAny dest ["buitenlands", "buiten"] = weight * 0.75

-- Pattern matching on BotState and input
handleInput :: BotState -> String -> (String, BotState) -- Takes current state and user input, returns response and next state
handleInput state input -- Binds parameters state and input
    | containsAny input ["help", "hulp"] = (provideHelp state, state) -- If user input is "help", provide contextual help based on current state and remain in the same state	
    | containsAny input ["exit", "stop"] = (R.msgExit, Idle) -- Returns exit message and sets state to Idle

-- IDLE state
handleInput Idle input -- Only checks input for these keywords when in Idle state
    | containsAny input ["waar", "wanneer", "pakket", "trackingnummer"] = (R.msgAskTracking, WaitingForTracking)
    | containsAny input ["hallo", "hi", "dag", "hoi"]      = (R.msgGreeting, Idle)
    | containsAny input ["kost", "prijs", "verzending", "verzenden"] = (R.msgAskShipping, WaitingForKgPackage)
    | otherwise                                     = (R.msgUnknown, Idle)

handleInput WaitingForKgPackage input
    | isValidKg input = let weight = parseKg input in (R.msgAskDestination, WaitingForDestinationPackage weight)
    | otherwise = (R.msgInvalidKG, WaitingForKgPackage)

handleInput (WaitingForDestinationPackage weight) input
    | isValidDestination input = (R.msgCalculateShipping, WaitingForShippingCalculation weight input)
    | otherwise         = (R.msgInvalidShipping, WaitingForDestinationPackage weight)

-- TRACKING state
handleInput WaitingForTracking input
    | isValidTracking input = (R.msgWaiting, WaitingForPackageStatus)
    | containsAny input ["waar", "vinden"] = (R.helpTracking, WaitingForTracking)
    | containsAny input ["niet", "geen"] = (R.msgAskPostalCode, WaitingForPostalCode)
    | otherwise         = (R.msgAskTracking, WaitingForTracking)

-- POSTAL CODE state
handleInput WaitingForPostalCode input
    | isValidPostal input = (R.msgAskHouseNumber, WaitingForHouseNumber)
    | otherwise           = (R.msgInvalidPostal, WaitingForPostalCode)

-- HOUSE NUMBER state
handleInput WaitingForHouseNumber input
    | any isDigit input = (R.msgWaiting, WaitingForPackageStatus)
    | otherwise         = (R.msgInvalidHouse, WaitingForHouseNumber)

provideHelp :: BotState -> String
provideHelp state = case state of --Based on BotState, return corresponding help message
    Idle                 -> R.helpIdle
    WaitingForTracking   -> R.helpTracking
    WaitingForPostalCode -> R.helpPostal
    WaitingForHouseNumber -> R.helpHouse
    WaitingForKgPackage -> R.helpKgPackage

-- Recursion: The function calls itself to implement loop: runBot nextState.
runBot :: BotState -> IO ()
runBot state = do
    input <- getLine -- Get user input
    let (response, nextState) = handleInput state input
    case nextState of
        WaitingForPackageStatus -> do
            -- Acknowledge the check and simulate a short processing delay
            putStrLn $ "PostNL: " ++ response
            -- Decide outcome randomly
            chance <- randomRIO (1, 100 :: Int)
            let packageStatusResponse = getChanceOutcome chance
            threadDelay 1500000  -- 1.5s pause to simulate lookup
            putStrLn $ "PostNL: " ++ packageStatusResponse
            runBot Idle

        WaitingForShippingCalculation weight dest -> do
            putStrLn $ "PostNL: " ++ response
            let shippingCost = calculateShipping weight dest
            threadDelay 1500000
            putStrLn $ "PostNL: De verzendkosten voor uw pakket van " ++ printf "%.2f" weight ++ "kg zijn " ++ printf "%.2f" shippingCost ++ " euro."
            runBot Idle

        _ -> do
            putStrLn $ "PostNL: " ++ response
            -- Terminate recursion if user exits, otherwise continue with nextState
            unless (containsAny input ["exit", "stop"]) $
                runBot nextState

main :: IO ()
main = do
    putStrLn R.msgWelcome
    runBot Idle