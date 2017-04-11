module Main where

import SuperCow

{-# OPTIONS_GHC -Wall #-}

main :: IO ()
main = do
  images <- loadImages
  runSuperCow images
