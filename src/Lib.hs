module Lib (
    someFunc,
) where

import Data.Text.IO as TI
import RIO

someFunc :: IO ()
someFunc = TI.putStrLn "someFunc"
