module Main where

import Config.Credential

main :: IO ()
main = do
   cred <- Config.Credential.loadFromFile "/home/osamu/data/credentials.yml"
   print cred
