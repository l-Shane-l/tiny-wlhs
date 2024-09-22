
module Main where

import TinyWL.FFI
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    result <- c_main (fromIntegral $ length args) =<< withCArgs args
    putStrLn $ "TinyWL main returned: " ++ show result
