module Main where

import Network.SSH.Maesshtro

main :: IO ()
main = putStrLn $ show $ runSession dummyServer $ run "echo 42"
