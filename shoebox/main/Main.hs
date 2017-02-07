{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Yesod
import WebBackend
import Data.IORef
import Data.Maybe
import Shoebox.Interface

main :: IO ()
main = do
	shoeDB <- shoeReadDB "frz"
	ref <- newIORef (fromJust shoeDB)
	warp 3000 (ShoeWeb ref)
