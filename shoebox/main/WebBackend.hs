{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, TypeSynonymInstances, StandaloneDeriving, ViewPatterns, TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings #-}

module WebBackend where

    import Yesod
    import Shoebox.Data
    import Shoebox.Interface
    import Shoebox.Data

    import qualified Data.Text as T
    import Data.Typeable
    import Data.Data
    import Data.Aeson
    import Text.Julius(rawJS)
    import Data.IORef
    import Data.Maybe

    import System.Directory (getDirectoryContents)
    import System.FilePath (dropExtension, takeBaseName)
    import Data.List.Split (splitOn)
    import qualified System.IO.Strict as S
    import qualified Data.Map as M

    import Control.Exception (catch, SomeException)



-- setup basic types and routes

    data ShoeWeb = ShoeWeb (IORef ShoeDB)

    instance Yesod ShoeWeb

    mkYesod "ShoeWeb" [parseRoutes|
      / HomeR GET

      /api/db/listDBs DatabasesR GET
      /api/db/readDB/#T.Text DatabaseR GET
      /api/db/writeDB/#T.Text DatabaseWrR GET
      /api/db/deleteDB/#T.Text DatabaseDelR POST

      /api/query/word QueryR POST

      /api/write/segDB WriteSegR POST
      /api/write/lexDB WriteLexR POST
      /api/write/prefixDB WritePrefixR POST
      /api/write/suffixDB WriteSuffixR POST

 |]

--    import Yesod.Core.Widget

    -- WIDGETS
    -- -------

    mainWidget :: Widget
    mainWidget = do
        toWidget [hamlet|
            <body>
                <h1>shoebox backend
        |]

-- LOGIC
-- -----

    getHomeR :: Handler Html
    getHomeR = defaultLayout $ do
        mainWidget

    getDatabasesR :: Handler Value
    getDatabasesR = do
        dbs <- liftIO shoeListDBs
        return $ toJSON dbs

    getDatabaseR :: T.Text -> Handler Value
    getDatabaseR dbName = do
        ShoeWeb ref <- getYesod
        shoeDB <- liftIO $ shoeReadDB dbName
        case shoeDB of
            Just db -> do
                            liftIO $ writeIORef ref db
                            return $ toJSON True
            Nothing -> return $ toJSON False

    getDatabaseWrR :: T.Text -> Handler ()
    getDatabaseWrR dbName = do
        ShoeWeb ref <- getYesod
        shoeDB <- liftIO $ readIORef ref
        f <- liftIO $ shoeWriteDB shoeDB dbName
        return ()

    postDatabaseDelR :: T.Text -> Handler ()
    postDatabaseDelR dbName = do
        f <- liftIO $ shoeDeleteDB dbName
        return ()

    postQueryR :: Handler Value
    postQueryR = do
        q <- requireJsonBody :: Handler T.Text
        ShoeWeb ref <- getYesod
        shoeDB <- liftIO $ readIORef ref
        let rval = shoeQueryDB shoeDB q
        returnJson rval

    postWriteSegR :: Handler ()
    postWriteSegR = do
        (k, val) <- requireJsonBody :: Handler (T.Text, [MorphemeBreak])
        ShoeWeb ref <- getYesod
        shoeDB <- liftIO $ readIORef ref
        let shoeDB' = shoeWriteSegDB shoeDB k val
        liftIO $ writeIORef ref shoeDB'
        return ()

    postWriteLexR :: Handler ()
    postWriteLexR = do
        (k, val) <- requireJsonBody :: Handler (T.Text, [T.Text])
        ShoeWeb ref <- getYesod
        shoeDB <- liftIO $ readIORef ref
        let shoeDB' = shoeWriteLexDB shoeDB k val
        liftIO $ writeIORef ref shoeDB'
        return ()

    postWritePrefixR :: Handler ()
    postWritePrefixR = do
        (k, val) <- requireJsonBody :: Handler (T.Text, [T.Text])
        ShoeWeb ref <- getYesod
        shoeDB <- liftIO $ readIORef ref
        let shoeDB' = shoeWritePrefixDB shoeDB k val
        liftIO $ writeIORef ref shoeDB'
        return ()

    postWriteSuffixR :: Handler ()
    postWriteSuffixR = do
        (k, val) <- requireJsonBody :: Handler (T.Text, [T.Text])
        ShoeWeb ref <- getYesod
        shoeDB <- liftIO $ readIORef ref
        let shoeDB' = shoeWriteSuffixDB shoeDB k val
        liftIO $ writeIORef ref shoeDB'
        return ()






