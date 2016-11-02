module Main (main) where

import Yesod
import GUI

main :: IO ()
main = do
	shoeDB <- readFrenchDB
	warp 3000 (ShoeWeb shoeDB)
