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
      /databases DatabasesR GET
      /database/#T.Text DatabaseR GET POST
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
                            <div onclick="$('#dlgImport').dialog('open')"">Import
                            <div data-options="iconCls:'icon-save'" onclick="$('#dlgSave').dialog('open')">Save
                            <div onclick="$('#dlgAbout').dialog('open')">About

                        <div id="dlgAbout" class="easyui-dialog" title="About" style="width:400px;height:200px;padding:10px;" data-options="modal:true,closed:true">
                            Shoebox Program (c) 2017 by Frankfurt Haskell User Group

                        <div id="dlgOpen" class="easyui-dialog" title="Open Database" style="width:400px;height:200px;padding:10px;" data-options="modal:true,closed:true">
                            <p>
                            Refresh file list: 
                            <button class="easyui-linkbutton" data-options="iconCls:'icon-reload'" onclick="updateFiles()" style="width:80px">Reload</button>
                            <p>
                            Select File, to open: 
                            <input #openCombo class="easyui-combobox" data-options="onChange:openFile">
                            <p>
                            <div #openResult>

                        <div id="dlgImport" class="easyui-dialog" title="Import Shoebox File" style="width:400px;height:200px;padding:10px;" data-options="modal:true,closed:true">
                            Import existing shoebox data file:<p>
                            <input #importFile class="easyui-filebox" style="width:300px" data-options="prompt:'Choose Shoebox File',accept:'*.u8',onChange:importFile">
                            <div #resultImportFile>

                        <div id="dlgSave" class="easyui-dialog" title="Save" style="width:400px;height:200px;padding:10px;" data-options="modal:true,closed:true">
                            <p>
                            Sure to save data: 
                            <button class="easyui-linkbutton" data-options="iconCls:'icon-save'" onclick="saveFile()" style="width:80px">Save</button>
                            <p>
                            <div #saveResult>

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

            function importFile(fname) {
                $.get("/database/" + fname, function(result) {
                        if (result) {
                            $("#resultImportFile").html("<b>successfully</b> imported data!");
                            $("#file").html("opened file: <b>" + fname + "</b>");
                        } else {
                            $("#resultImportFile").html("<b>error occurred</b> during import of data!");
                        }
                    });
            }

            function openFile(fname, oldValue) {
                $("#openResult").html("");
                $.get("/database/" + fname, function(result) {
                        if (result) {
                            $("#openResult").html("<b>successfully</b> imported data!");
                            $("#file").html("opened file: <b>" + fname + "</b>");
                        } else {
                            $("#openResult").html("<b>error occurred</b> during import of data!");
                        }
                    });
            }

            function saveFile() {
                var inf = $("#file").html();
                inf = inf.replace("opened file: <b>", "");
                var fname = inf.replace("</b>", "");

                $.post("/database/" + fname, function(result) {
                        $("#saveResult").html("");
                        console.log(result);
                        if (result.length > 0) {
                            $("#saveResult").html("<b>successfully</b> saved data!");
                            $("#file").html("opened file: <b>" + result + "</b>");
                        } else {
                            $("#saveResult").html("<b>error occurred</b> during save data!");
                        }
                    });
            }

            function updateFiles() {
                $.get("/databases", function(result) {
                        if (result) {
                            var newData = [];
                            for(var i = 0; i < result.length; i++) {
                                var opt = result[i];
                                newData.push({value:opt, text:opt});
                            };
                            $("#openCombo").combobox("loadData", newData);
                        } else {
                        }
                    });
            }

            $(function(){updateFiles();});

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


    -- data file handling, 
    --  all data files are in "data"
    --  

    -- dbFiles - gives back a list of existing db files from "data" 
    dbFiles :: IO [String]
    dbFiles = do
        -- first check files in data, with db extension
        files <- getDirectoryContents "data"
        let dbs = (filter isSbx files)
        return dbs

    -- open file, create file, save file
    openFile :: String -> IO (Maybe ShoeDB)
    openFile fname = do
        catchAny (do
            if isSbx fname 
                then do
                    readData <- S.readFile $ "data/" ++ fname
                    return (Just (read readData))
                else do
                    db' <- loadShoeDB $ "data/" ++ (dropExtension fname)
                    return (Just db')
                    ) $ \e -> return Nothing

    -- save file under name, with sbx extension
    saveFile :: String -> ShoeDB -> IO String
    saveFile fname db = do
        let writeName = if isSbx fname 
                            then fname 
                            else ((takeBaseName fname) ++ ".sbx")
        catchAny (do
                writeFile ("data/" ++ writeName) (show db)
                return writeName) $ \e -> return ""

    -- file handling helpers
    isSbx :: String -> Bool
    isSbx name = case reverse (splitOn "." name) of 
        (x:xs) -> x == "sbx"
        _ -> False

    historicDBs = M.fromList [("French", "frz")]

    catchAny :: IO a -> (SomeException -> IO a) -> IO a
    catchAny = Control.Exception.catch



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


    -- return names of all available databases
    getDatabasesR :: Handler Value
    getDatabasesR = do
        liftIO $ print "getDatabases"
        dbs <- liftIO dbFiles
        return $ toJSON dbs

    -- set the actual database, load it
    getDatabaseR :: T.Text -> Handler Value
    getDatabaseR dbName = do
        liftIO $ print ("getDatabase: " ++ (T.unpack dbName))
        ShoeWeb ref <- getYesod
        shoeDB <- liftIO $ openFile (T.unpack dbName)
        case shoeDB of
            Just db -> do
                            liftIO $ writeIORef ref db
                            return $ toJSON True
            Nothing -> return $ toJSON False

    -- save the actual database back to the data
    postDatabaseR :: T.Text -> Handler Value
    postDatabaseR dbName = do
        liftIO $ print ("postDatabase: " ++ (T.unpack dbName))
        ShoeWeb ref <- getYesod
        shoeDB <- liftIO $ readIORef ref
        f <- liftIO $ saveFile (T.unpack dbName) shoeDB
        return (toJSON f)




