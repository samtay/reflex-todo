module Main where

import           Backend.App    (app)
import           Backend.Data   (openState)
import           Backend.Server (runServer)

main :: IO ()
main = do
  state <- openState
  runServer 3000 (app state)

-- TODO Refactor to have an initApp todoState serverState etc that starts up a monad transformer
