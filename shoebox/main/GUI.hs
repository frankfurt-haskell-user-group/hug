{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, TypeSynonymInstances, StandaloneDeriving, ViewPatterns, TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings #-}

module GUI where

    import Yesod
    import Shoebox.Data
    import Shoebox.Basics
    import Shoebox.Parser

    import qualified Data.Text as T
    import Data.Typeable
    import Data.Data
    import Data.Aeson
    import Text.Julius(rawJS)
    import Data.IORef
    import Data.Maybe

    import System.Directory
    import Data.List.Split (splitOn)
    import qualified Data.Map as M

-- setup basic types and routes

    data ShoeWeb = ShoeWeb (IORef ShoeDB)

    instance Yesod ShoeWeb

    mkYesod "ShoeWeb" [parseRoutes|
      / HomeR GET
      /databases DatabasesR GET
      /database/#T.Text DatabaseR GET POST DELETE
      /script/#T.Text ScriptR GET
      /script/images/#T.Text ImagesR GET
      /query/#T.Text QueryR GET
    |]

--    import Yesod.Core.Widget

    -- WIDGETS
    -- -------

    mainWidget :: Widget
    mainWidget = do

        addScriptRemote "http://code.jquery.com/jquery-1.6.min.js"
        addScriptRemote "http://www.jeasyui.com/easyui/jquery.easyui.min.js"

        addStylesheetRemote "http://www.jeasyui.com/easyui/themes/default/easyui.css"
        addStylesheetRemote "http://www.jeasyui.com/easyui/themes/icon.css"
        addStylesheetRemote "http://www.jeasyui.com/easyui/themes/color.css"
        addStylesheetRemote "http://www.jeasyui.com/easyui/demo/demo.css"

        setTitle "The Cool Shoebox Program"

        toWidget [hamlet|

            <body style="padding:0px;">
                <div id="cc" class="easyui-layout" style="width:100%;height:600px;padding:0px;">

                    <div data-options="region:'north', title:'The Cool Shoebox Program'" style="height:50px;">
                        <div #file>
                            opened file: <b>French</b>

                    <div data-options="region:'west',split:true" style="width:150px;height:100%;">
                        <div id="mm" class="easyui-menu" data-options="inline:true" style="width:100%;">
                            <div onclick="$('#dlgOpen').dialog('open')"">Open
                            <div data-options="iconCls:'icon-save'" onclick="$('#dlgSave').dialog('open')">Save
                            <div onclick="$('#dlgAbout').dialog('open')">About

                        <div id="dlgAbout" class="easyui-dialog" title="About" style="width:400px;height:200px;padding:10px;" data-options="modal:true,closed:true">
                            Shoebox Program (c) 2017 by Frankfurt Haskell User Group

                        <div id="dlgOpen" class="easyui-dialog" title="Open Database" style="width:400px;height:200px;padding:10px;" data-options="modal:true,closed:true">
                            Open File Dialog

                        <div id="dlgSave" class="easyui-dialog" title="Save" style="width:400px;height:200px;padding:10px;" data-options="modal:true,closed:true">
                            Save File Dialog

                    <div data-options="region:'center'">
                        <div id="tt" class="easyui-tabs" style="width:100%;height:100%;">
                            <div title="Browse" style="padding:20px;display:none;">
                                Browser
                            <div title="Interlinearisation" style="overflow:auto;padding:20px;display:none;">
                                Interlinear
                            <div title="Query" style="display:none;">
                                <p>
                                Here you can evaluate queries, type in the query in the textbox below and press "Enter". For example
                                try the words "maison" or "abattue".
                                <p>
                                Type query here:
                                <p>
                                <input #queryText class="easyui-textbox" style="width:500px" data-options="onChange:sendQuery">
                                <div #resultText style="font-size:90%;">

        |]


        toWidget [julius|

            function sendQuery() {
                qt = $('#queryText').val();
                if (qt != "") {
                    $.get("/query/" + qt, function (msg) {
                        $("#resultText > *").remove();
                        $("#resultText").append(msg);
                      });
                } else {
                    $("#resultText > *").remove();
                }
            }

        |]

-- OLD


    databaseWidget :: Widget
    databaseWidget = do
        toWidget [julius|
            $(function () {
                var dbselect = $().w2field("list", {name: "dbselect"});
                $.ajax({
                    method: "GET",
                    url: ("/databases"),
                    headers: {          
                        Accept: "application/json",         
                        }
                })
                  .done(function( msg ) {

                        var options = []; 
                        for (i = 0; i < msg.length; i++) {
                            dbselect.items.push(msg[i]);
                        }
                        dbselect.change = function( event, ui ) {
                                    $.ajax({
                                        method: "GET",
                                        url: ("/database/" + $(this).val()),
                                        headers: {          
                                            Accept: "application/json",         
                                            }
                                    })
                              }
                  });

            });
            |]


    browseWidget :: Widget
    browseWidget = do
        toWidget [hamlet|
            browse content here
        |]

    interlinearWidget :: Widget
    interlinearWidget = do
        toWidget [hamlet|
            interlinearisation content here
        |]

-- LOGIC
-- -----

    -- pretty print an interlinear block
    ppIlb :: [InterlinearBlock] -> Html
    ppIlb ilbs = do [shamlet|
            <h3>Query Result
            <table class="easyui-datagrid" style="width:100%">
                <thead>
                    <td>Input Text
                    <td>Morpheme Break
                    <td>Gloss

                $forall ilb <- ilbs
                    $with ILB (TX it) (MB ml) (GL cl) <- ilb
                        <tr>
                             <td>#{show it}
                             <td>#{show ml}
                             <td>#{show cl}
                |]


-- OLD


    -- file handling

    historicDBs = M.fromList [("French", "frz")]

    loadHistoricDB dbName = case M.lookup dbName historicDBs of
        Just fn -> do
            db <- loadShoeDB $ "data/" ++ fn
            return $ Just db
        Nothing -> return Nothing

    isSbx :: String -> Bool
    isSbx name = case reverse (splitOn "." name) of 
        (x:xs) -> x == "sbx"
        _ -> False

    otherDBs :: IO [String]
    otherDBs = do
        files <- getDirectoryContents "data"
        let dbs = filter isSbx files
        return dbs

    allDBs = do
        news <- otherDBs
        let olds = M.keys historicDBs
        return (news ++ olds)

    loadDB name = do
        -- first check history
        db <- loadHistoricDB name
        case db of
            Just shoeDB -> return shoeDB
            _ -> readDB name

    readDB name = do
        readData <- readFile $ "data/" ++ name
        let reconstructed = read readData :: ShoeDB   
        return reconstructed

    writeDB name db = do
        let writeName = if isSbx name then name else (name ++ ".sbx")
        writeFile ("data/" ++ writeName) (show db)


-- ROUTE HANDLER
-- -------------

    -- serve home page
    getHomeR = defaultLayout $ do
        mainWidget

    -- serve static files
    getScriptR f = getFile f ""
    getImagesR f = getFile f "images"

    getFile f p = defaultLayout $ do
        case (reverse . take 3 . reverse $ T.unpack f) of
            "png" -> sendFile typePng ("jquery-ui/" ++ p ++ "/" ++ (T.unpack f))
            "css" -> sendFile typeCss ("jquery-ui/" ++  p ++ "/" ++ (T.unpack f))
            ".js" -> sendFile typeJavascript ("jquery-ui/" ++ p ++ "/" ++  (T.unpack f))
            _ -> sendFile typePlain ("jquery-ui/" ++ p ++ "/" ++ (T.unpack f))
        return ()

    -- deliver pretty printed interlinear block
    getQueryR :: T.Text -> Handler Html
    getQueryR q = do
        ShoeWeb ref <- getYesod
        shoeDB <- liftIO $ readIORef ref
        let rval = concat (intl q shoeDB)
        return $ ppIlb rval



-- OLD


    -- return names of all available databases
    getDatabasesR :: Handler TypedContent
    getDatabasesR = do
        liftIO $ print "getDatabases"
        dbs <- liftIO allDBs
        selectRep $ do
            provideJson dbs

    -- set the actual database, load it
    getDatabaseR :: T.Text -> Handler Html
    getDatabaseR dbName = defaultLayout $ do
        liftIO $ print ("getDatabase: " ++ (T.unpack dbName))
        ShoeWeb ref <- getYesod
        shoeDB <- liftIO $ loadDB (T.unpack dbName)
        liftIO $ writeIORef ref shoeDB
        [whamlet| |]

    -- save the actual database back to the data
    postDatabaseR :: T.Text -> Handler Html
    postDatabaseR dbName = defaultLayout $ do
        liftIO $ print ("postDatabase: " ++ (T.unpack dbName))
        ShoeWeb ref <- getYesod
        shoeDB <- liftIO $ readIORef ref
        liftIO $ writeDB (T.unpack dbName) shoeDB
        [whamlet| |]

    deleteDatabaseR :: T.Text -> Handler Html
    deleteDatabaseR db = defaultLayout [whamlet| <h1>cool |]



