module Main where

import qualified SSSS
import qualified PrimeField
import qualified Command

main :: IO ()
main = Command.run
-- main = SSSS.test (Nothing :: Maybe PrimeField.Secret128)
