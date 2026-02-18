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
import Data.List (isInfixOf)

containsAny :: String -> [String] -> Bool
containsAny input = any (`isInfixOf` map toLower input)

containsAll :: String -> [String] -> Bool
containsAll input = all (`isInfixOf` map toLower input)

clearWhiteSpace :: String -> String
clearWhiteSpace = filter (not . isSpace)

isValidDestination :: String -> Bool
isValidDestination input = containsAny input ["binnenlands", "binnen", "buitenlands", "buiten"]

isValidPostal :: String -> Bool
isValidPostal input = case clearWhiteSpace input of
    [d1,d2,d3,d4,l1,l2] -> all isDigit [d1,d2,d3,d4] && all isAlpha [l1,l2]
    _                   -> False

isValidTracking :: String -> Bool
isValidTracking input = case clearWhiteSpace input of
    [d1,d2,d3,d4,d5,d6] -> all isDigit [d1,d2,d3,d4,d5,d6]
    _                   -> False

isValidKg :: String -> Bool
isValidKg input =
    let cleaned = clearWhiteSpace input
    in not (null cleaned)
       && all (\c -> isDigit c || c == '.') cleaned
       && case reads cleaned :: [(Double, String)] of
            [(w, rest)] -> null rest
            _           -> False

-- Parse kg; caller should ensure input is valid with isValidKg
parseKg :: String -> Double
parseKg input =
    case reads (clearWhiteSpace input) :: [(Double, String)] of
         [(weight, _)] -> weight
         _        -> 0.0

calculateShipping :: Double -> String -> Double
calculateShipping weight dest
    | containsAny dest ["binnenlands", "binnen"] = weight * 0.75 + 2.5
    | containsAny dest ["buitenlands", "buiten"] = weight * 1.0 + 3.5