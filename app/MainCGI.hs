module Main where

import Lib
import Network.Wai.Handler.CGI
import Network.Wai.Middleware.Timeout

main :: IO ()
main = run $ timeout 2 app
