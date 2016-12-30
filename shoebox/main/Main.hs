module Main (main) where

import Yesod
import GUI
import Data.IORef
import Data.Maybe

main :: IO ()
main = do
	shoeDB <- openFile "frz.sbx"
	ref <- newIORef (fromJust shoeDB)
	warp 3000 (ShoeWeb ref)
