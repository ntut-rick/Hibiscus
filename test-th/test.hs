{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Hibiscus.Asm2

main =
  do
    putStrLn ""
    putStrLn $ emit $ OpStore (Id 1) (Id 2)
    putStrLn $ emit $ OpIAdd (Id 1) (Id 2) (Id 3) (Id 4)
