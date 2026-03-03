module Utils (
    containsAny,
    containsAll,
    clearWhiteSpace,
    isValidDestination,
    isValidPostal,
    isValidTracking,
    isValidKg,
    parseKg,
    calculateShipping
    ) where

import Data.Char (isDigit, isAlpha, isSpace, toLower)
import Data.List (isInfixOf, isSuffixOf)
import Data.Maybe (isJust)
-- Higher-order function: any/all check if keyword exists in user input and takes function isInfixOf as an argument.
containsAny :: String -> [String] -> Bool --Function takes a string, then a list of strings, and returns a boolean.
containsAny input = any (`isInfixOf` map toLower input) --isInfixOf checks if input matches any keyword in the list. Input is set to lowercase to prevent case sensitivity issues.

containsAll :: String -> [String] -> Bool
containsAll input = all (`isInfixOf` map toLower input)
-- Higher-order function: filter
clearWhiteSpace :: String -> String
clearWhiteSpace = filter (not . isSpace)

isValidDestination :: String -> Bool
isValidDestination input = containsAny input ["binnenlands", "binnen", "buitenlands", "buiten"]
-- Pattern matching: on list of characters to validate postal code and tracking number formats
isValidPostal :: String -> Bool
isValidPostal input = case clearWhiteSpace input of -- Filter: keeps only non-space characters from input. Case: tries to match input against pattern.
    [d1,d2,d3,d4,l1,l2] -> all isDigit [d1,d2,d3,d4] && all isAlpha [l1,l2] -- Binds first 4 chars to d1-d4 and checks if they are digits, binds last 2 chars to l1-l2 and checks if they are letters.
    _                   -> False -- Any input that doesn't match the pattern will return false.

isValidTracking :: String -> Bool
isValidTracking input = case clearWhiteSpace input of
    [d1,d2,d3,d4,d5,d6] -> all isDigit [d1,d2,d3,d4,d5,d6]
    _                   -> False

-- NormalizeKg: helper to clean and standardize input.
normalizeKg :: String -> String
normalizeKg input =
    let stripped = clearWhiteSpace input
        normalizedComma = map (\c -> if c == ',' then '.' else c) stripped -- Replace comma with dot
        lowered = map toLower normalizedComma -- Convert to lowercase for consistent suffix checking
    in if "kg" `isSuffixOf` lowered then take (length lowered - 2) lowered else lowered -- Remove "kg" if present

-- Parse kg: parses the input (string) into a Maybe Double.
parseKg :: String -> Maybe Double
parseKg input =
    case reads (normalizeKg input) :: [(Double, String)] of -- Reads tries to parse the normalized input as a Double.
         [(weight, rest)] -> if null rest && weight >= 0 then Just weight else Nothing
         _                -> Nothing

isValidKg :: String -> Bool
isValidKg input = isJust (parseKg input)

calculateShipping :: Double -> String -> Double
calculateShipping weight dest
    | containsAny dest ["binnenlands", "binnen"] = weight * 0.75 + 2.5
    | containsAny dest ["buitenlands", "buiten"] = weight * 1.0 + 3.5