module Main (main) where

import Yesod
import GUI
import Data.IORef

main :: IO ()
main = do
	shoeDB <- loadDB "French"
	ref <- newIORef shoeDB
	warp 3000 (ShoeWeb ref)
