{-# LANGUAGE BangPatterns #-}

module Main where

import TestEverything (testEverything)


main :: IO ()
main = testEverything "test/exmaple.hi"
