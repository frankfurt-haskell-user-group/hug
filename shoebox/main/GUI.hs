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

      -- data queries
      /lexicon AllLexR GET
      /lexicon/#T.Text LexR GET

      /suffix AllSuffR GET
      /prefix AllPreR GET
      /seg AllSegR GET
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

            <body style="padding:0px;overflow:hidden;height:800px">
                <div id="cc" class="easyui-layout" style="width:100%;height:100%;padding:0px;">

                    <div data-options="region:'north'" style="width:100%; height:50px;">

                        <a href="#" class="easyui-menubutton" data-options="menu:'#mm1',iconCls:'icon-save'">File
                        <div id="mm1" class="easyui-menu">
                            <div onclick="$('#dlgOpen').dialog('open')"">Open
                            <div onclick="$('#dlgImport').dialog('open')"">Import
                            <div onclick="$('#dlgSave').dialog('open')">Save

                        <a href="#" class="easyui-menubutton" data-options="menu:'#mm2'">About
                        <div id="mm2" class="easyui-menu">
                            <div onclick="$('#dlgAbout').dialog('open')">About

                    <div data-options="region:'center'" style="height:calc(100% - 100px);width:100%">
                        <div id="tt" class="easyui-tabs">
                            <div title="Browse" style="padding:20px;display:none;">
                                <p>
                                This is the database browser. Load the data with the button below and inspect details by clicking on a specific item.
                                <p>
                                <button class="easyui-linkbutton" onclick="loadBrowserData()" style="width:80px">Load Data</button>
                                <p>
                                <div #browserData class="easyui-datalist" title="Lexicon" data-options="onSelect:browserDataSelect" style="width:300px;height:300px">
                                <p>
                                <div class="easyui-panel" style="width:300px;height:200px;" #browserText>Details shown here, once selected

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

                    <div data-options="region:'south', title:'The Cool Shoebox Program'" style="height:50px;">
                        <div #file>
                            opened file: <b>frz.sbx</b>

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

        |]


        toWidget [julius|

            function loadBrowserData() {
                $("#browserText").contents().remove();

                $.get("/lexicon", function(result) {
                        if (result) {
                            var newData = [];
                            for(var i = 0; i < result.length; i++) {
                                var opt = result[i];
                                newData.push({value:opt, text:opt});
                            };
                            $("#browserData").datalist("loadData", newData);

                        } else {
                            $("#browserText").append("<b>error occurred</b> during data-load!");
                        }
                    });
            }

            function browserDataSelect(sel, row)
            {
                $("#browserText").contents().remove();

                $.get("/lexicon/" + row.text, function(result) {
                        if (result) {
                            for(var i = 0; i < result.length; i++) {
                                var opt = result[i];
                                $("#browserText").append(opt + "<br>\n");
                            };
                        } else {
                            $("#browserText").append("<b>error occurred</b> during selection data-load!");
                        }
                    });
            }

            function sendQuery() {
                qt = $('#queryText').val();
                if (qt != "") {
                    $.get("/query/" + qt, function (msg) {
                        $("#resultText").contents().remove();
                        $("#resultText").append(msg);
                      });
                } else {
                    $("#resultText").contents().remove();
                }
            }

            function importFile(fname) {
                $("#resultImportFile").contents().remove();
                $.get("/database/" + fname, function(result) {
                        if (result) {
                            $("#resultImportFile").append("<b>successfully</b> imported data!");
                            $("#file").html("opened file: <b>" + fname + "</b>");
                        } else {
                            $("#resultImportFile").append("<b>error occurred</b> during import of data!");
                        }
                    });
            }

            function openFile(fname, oldValue) {
                $("#openResult").contents().remove();
                $.get("/database/" + fname, function(result) {
                        if (result) {
                            $("#openResult").append("<b>successfully</b> imported data!");
                            $("#file").html("opened file: <b>" + fname + "</b>");
                        } else {
                            $("#openResult").append("<b>error occurred</b> during import of data!");
                        }
                    });
            }

            function saveFile() {
                var inf = $("#file").html();
                inf = inf.replace("opened file: <b>", "");
                var fname = inf.replace("</b>", "");

                $.post("/database/" + fname, function(result) {
                        $("#saveResult").contents().remove();
                        if (result.length > 0) {
                            $("#saveResult").append("<b>successfully</b> saved data!");
                            $("#file").html("opened file: <b>" + result + "</b>");
                        } else {
                            $("#saveResult").append("<b>error occurred</b> during save data!");
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

            $(function(){
                updateFiles();
                self.resizeTo(1000,840);
                });

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

    -- Lexicon part of database
    getLexR :: T.Text -> Handler Value
    getLexR q = do
        ShoeWeb ref <- getYesod
        (lexDB, _, _, _) <- liftIO $ readIORef ref
        liftIO $ print $ "getLexR: " ++ (T.unpack q)
        case M.lookup q lexDB of
            Just val -> return (toJSON val)
            Nothing -> notFound

    -- Lexicon part of database
    getAllLexR :: Handler Value
    getAllLexR = do
        ShoeWeb ref <- getYesod
        (lexDB, _, _, _) <- liftIO $ readIORef ref
        liftIO $ print "getAllLexR"
        return (toJSON (M.keys lexDB))

    -- Suffix part of database
    getAllSuffR :: Handler Value
    getAllSuffR = do
        ShoeWeb ref <- getYesod
        (_, _, suff, _) <- liftIO $ readIORef ref
        return (toJSON suff)

    -- Prefix part of database
    getAllPreR :: Handler Value
    getAllPreR = do
        ShoeWeb ref <- getYesod
        (_, pre, _, _) <- liftIO $ readIORef ref
        return (toJSON pre)

    -- Segmentation part of database
    getAllSegR :: Handler Value
    getAllSegR = do
        ShoeWeb ref <- getYesod
        (_, _, _, seg) <- liftIO $ readIORef ref
        return (toJSON seg)

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




