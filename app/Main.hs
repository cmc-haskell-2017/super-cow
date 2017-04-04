module Main where

import Lib

{-# OPTIONS_GHC -Wall #-}

main :: IO ()
main = do
	images <- loadImages
	runSuperCow images
