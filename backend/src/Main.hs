{-# LANGUAGE OverloadedStrings #-}
module Main where

import Backend.Server (runServer)

-- TODO Run db stuff here (groundhog? checkout something else?)
main :: IO ()
main = runServer 3000
