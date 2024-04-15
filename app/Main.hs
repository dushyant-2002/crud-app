module Main (main) where

import qualified Server (runServer)

main :: IO ()
main = Server.runServer
