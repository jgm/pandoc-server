module Main where

import Lib
import Network.Wai.Handler.CGI

main :: IO ()
main = run app
