module Main where

import SuperCow
import Interface

{-# OPTIONS_GHC -Wall #-}

main :: IO ()
main = do
  images <- loadImages
  runSuperCow images
