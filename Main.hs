module Main where
import qualified Responses as R
import Bot (runBot, BotState(Idle))

main :: IO ()
main = do
    putStrLn R.msgWelcome
    runBot Idle