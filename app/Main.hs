module Main where

import Lib
import Network.Wai.Logger
import Network.Wai.Handler.Warp

main :: IO ()
main = do
  withStdoutLogger $ \applogger -> do
    putStrLn "Starting server on port 8080..."
    let settings = setPort 8080 $ setLogger applogger defaultSettings
    runSettings settings app
