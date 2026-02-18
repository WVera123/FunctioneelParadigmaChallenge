module Bot (runBot, BotState(..)) where
import Data.Char (isDigit, isAlpha, isSpace, toLower)
import Data.List (isInfixOf)
import Control.Monad (unless)
import Control.Concurrent (threadDelay)
import System.Random (randomRIO)
import Text.Printf (printf)
import Responses
import Utils (containsAny, containsAll, isValidDestination, isValidPostal, isValidTracking, isValidKg, parseKg, clearWhiteSpace, calculateShipping)

-- Define states for a multi-step verification process
data BotState 
    = Idle 
    | WaitingForTracking 
    | WaitingForPostalCode 
    | WaitingForHouseNumber
    | WaitingForPackageStatus
    | WaitingForKgPackage
    | WaitingForDestinationPackage Double            -- carries weight
    | WaitingForShippingCalculation Double String    -- carries weight and destination

getChanceOutcome :: Int -> String
getChanceOutcome n
    | n <= 50   = msgInTransit    -- 50% In Transit
    | n <= 80   = msgOutForDelivery -- 30% Out for delivery within 1 day
    | n <= 95   = msgFinalDelay   -- 15% Delay
    | otherwise = msgFinalProblem -- 5% Missing/Problem

-- Pattern matching on BotState and input
handleInput :: BotState -> String -> (String, BotState) -- Takes current state and user input, returns response and next state
handleInput state input -- Binds parameters state and input
    | containsAny input ["help", "hulp"] = (provideHelp state, state) -- If user input is "help", provide contextual help based on current state and remain in the same state	
    | containsAny input ["exit", "stop"] = (msgExit, Idle) -- Returns exit message and sets state to Idle

-- IDLE state
handleInput Idle input -- Only checks input for these keywords when in Idle state
    | containsAny input ["waar", "wanneer", "aankomst", "trackingnummer"] = (msgAskTracking, WaitingForTracking)
    | containsAny input ["kwijt", "vinden", "bezorgd", "niet"] = (msgExcuseAskTracking, Idle)
    | containsAny input ["hallo", "hi", "dag", "hoi"]      = (msgGreeting, Idle)
    | containsAny input ["kost", "prijs", "verzending", "verzenden"] = (msgAskShipping, WaitingForKgPackage)
    | otherwise                                     = (msgUnknown, Idle)

handleInput WaitingForKgPackage input
    | isValidKg input = let weight = parseKg input in (msgAskDestination, WaitingForDestinationPackage weight)
    | otherwise = (msgInvalidKG, WaitingForKgPackage)

handleInput (WaitingForDestinationPackage weight) input
    | isValidDestination input = (msgCalculateShipping, WaitingForShippingCalculation weight input)
    | otherwise         = (msgInvalidShipping, WaitingForDestinationPackage weight)

-- TRACKING state
handleInput WaitingForTracking input
    | isValidTracking input = (msgWaiting, WaitingForPackageStatus)
    | containsAny input ["waar", "vinden"] = (helpTracking, WaitingForTracking)
    | containsAny input ["niet", "geen"] = (msgAskPostalCode, WaitingForPostalCode)
    | otherwise         = (msgAskTracking, WaitingForTracking)

-- POSTAL CODE state
handleInput WaitingForPostalCode input
    | isValidPostal input = (msgAskHouseNumber, WaitingForHouseNumber)
    | otherwise           = (msgInvalidPostal, WaitingForPostalCode)

-- HOUSE NUMBER state
handleInput WaitingForHouseNumber input
    | any isDigit input = (msgWaiting, WaitingForPackageStatus)
    | otherwise         = (msgInvalidHouse, WaitingForHouseNumber)

provideHelp :: BotState -> String
provideHelp state = case state of --Based on BotState, return corresponding help message
    Idle                 -> helpIdle
    WaitingForTracking   -> helpTracking
    WaitingForPostalCode -> helpPostal
    WaitingForHouseNumber -> helpHouse
    WaitingForKgPackage -> helpKgPackage

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