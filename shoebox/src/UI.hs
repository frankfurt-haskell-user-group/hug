module UI where

import Shoebox
import Graphics.QML
import Data.Text (Text)
import qualified Data.Text as T

startUI :: IO ()
startUI = do
    shoeboxClass <- newClass [
        defMethod' "getDictionaryText" (\_  -> do
                shoeDB <- loadShoeDB "frz"
                let (lex, suf, pref, seg) = shoeDB
                print "we are called"
                return (T.pack (show lex))
            ),
        defMethod' "displayText" (\_ txt -> do
--                let t = T.unpack txt
                print (txt::Text)
            ),
        defMethod' "onKey" (\_ key -> do
                let t = T.unpack key
                print "key pressed"
            ),
        defMethod' "factorial" (\_ txt ->
            let n = read $ T.unpack txt :: Integer
            in return . T.pack . show $ product [1..n] :: IO Text)
        ]

    ctx <- newObject shoeboxClass ()

    runEngineLoop defaultEngineConfig {
    initialDocument = fileDocument "shoebox.qml",
    contextObject = Just $ anyObjRef ctx}

    return ()