{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, TypeSynonymInstances, StandaloneDeriving, ViewPatterns, TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings #-}

module GUI where

    import Yesod
    import Shoebox
    import qualified Data.Text as T
    import Data.Typeable
    import Data.Data
    import Data.Aeson

-- setup basic types and routes

    data ShoeWeb = ShoeWeb ShoeDB

    instance Yesod ShoeWeb

    deriving instance Typeable Morpheme
    deriving instance Data Morpheme
    deriving instance Typeable MorphemeBreak
    deriving instance Data MorphemeBreak
    deriving instance Typeable ShoeDB
    deriving instance Data ShoeDB
    deriving instance Typeable TextLine
    deriving instance Data TextLine
    deriving instance Typeable GlossLine
    deriving instance Data GlossLine
    deriving instance Typeable Choice
    deriving instance Data Choice
    deriving instance Typeable Gloss
    deriving instance Data Gloss
    deriving instance Typeable InterlinearBlock
    deriving instance Data InterlinearBlock

    mkYesod "ShoeWeb" [parseRoutes|
      / HomeR GET
      /script/#T.Text ScriptR GET
      /script/images/#T.Text ImagesR GET
      /query/#T.Text QueryR GET
    |]

--    import Yesod.Core.Widget

    -- WIDGETS
    -- -------

    includeUI :: Widget
    includeUI = do
        addScriptRemote "http://code.jquery.com/jquery-3.1.1.js"
        addStylesheet $ ScriptR "jquery-ui.css"
        addScript $ ScriptR "jquery-ui.js"

    textWidget :: Widget
    textWidget = do
        includeUI
        toWidget [hamlet|
            <input type="text" #"myText" ."ui-widget" ."ui-state-default" ."ui-corner-all">
            |]
        toWidget [julius|
            $( "#myText" ).on("keyup", function() {
                  alert( "Handler for .change() called." );
                });;

            |]

    queryWidget :: Widget
    queryWidget = do
        includeUI
        toWidget [hamlet|
            <h2>shoebox query example
            Type in query text here:
            <input type="text" #"queryText" ."ui-widget" ."ui-state-default" ."ui-corner-all">
            <p>
            Query result:
            <div #"resultText" style="width:300px; height:200px">
            |]
        toWidget [julius|
            $( "#queryText" ).on("keyup", function() {

                qt = $( "#queryText" ).val();

                $.ajax({
                    method: "GET",
                    url: ("/query/" + qt),
                    headers: {          
                        Accept: "application/json",         
                        }
                })
                  .done(function( msg ) {
                    $("#resultText").text(msg);
                  });

                });;

            |]


{-

    includeW2UI :: Widget
    includeW2UI = do
        addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js"

        -- w2ui slightly modified, to make it work in edge, might be replaced by add..Remote
        addStylesheet $ ScriptR "w2ui-1.4.3.css"
        addScript $ ScriptR "w2ui-1.4.3.js"

    textWidget :: Widget
    textWidget = do
        includeW2UI
        toWidget [hamlet|
            <div><input id="form">
            |]
        toWidget [julius|
            $(function () {
                $('#form').w2form({ 
                    name     : 'form',
                    url      : 'server/post',
                    record: {
                        field_1 : 1,
                        field_2 : 2
                    },
                    fields: [
                        { name: 'field_1', type: 'text', required: true },
                        { name: 'field_2', type: 'alphanumeric', required: true }
                    ],
                    onChange: function (event) {
                        console.log(event);
                    }
                });                })
            |]

    gridWidget :: Widget
    gridWidget = do
        includeW2UI
        toWidget [hamlet|
            <div id="grid" style="width: 100%; height: 250px;"></div> 
        |]
        toWidget [julius|
            $(function () {
                $('#grid').w2grid({
                    name: 'grid',
                    header: 'List of Names',
                    columns: [
                        { field: 'fname', caption: 'First Name', size: '30%' },
                        { field: 'lname', caption: 'Last Name', size: '30%' },
                        { field: 'email', caption: 'Email', size: '40%' },
                        { field: 'sdate', caption: 'Start Date', size: '120px' }
                    ],
                    records: [
                        { recid: 1, fname: "Peter", lname: "Jeremia", email: 'peter@mail.com', sdate: '2/1/2010' },
                        { recid: 2, fname: "Bruce", lname: "Wilkerson", email: 'bruce@mail.com', sdate: '6/1/2010' },
                        { recid: 3, fname: "John", lname: "McAlister", email: 'john@mail.com', sdate: '1/16/2010' },
                        { recid: 4, fname: "Ravi", lname: "Zacharies", email: 'ravi@mail.com', sdate: '3/13/2007' },
                        { recid: 5, fname: "William", lname: "Dembski", email: 'will@mail.com', sdate: '9/30/2011' },
                        { recid: 6, fname: "David", lname: "Peterson", email: 'david@mail.com', sdate: '4/5/2010' }
                    ]
                });
                })
        |]

-}


-- LOGIC
-- -----

    readFrenchDB = do
        shoeDB <- loadShoeDB "frz"
        let allDBs = shoeDB
        return allDBs

    getGloss t allDBs = show $ gloss t allDBs


-- ROUTE HANDLER
-- -------------

    -- serve file
    getFile f p = defaultLayout $ do
        case (reverse . take 3 . reverse $ T.unpack f) of
            "png" -> sendFile typePng ("jquery-ui/" ++ p ++ "/" ++ (T.unpack f))
            "css" -> sendFile typeCss ("jquery-ui/" ++  p ++ "/" ++ (T.unpack f))
            ".js" -> sendFile typeJavascript ("jquery-ui/" ++ p ++ "/" ++  (T.unpack f))
            _ -> sendFile typePlain ("jquery-ui/" ++ p ++ "/" ++ (T.unpack f))
        return ()

--    deriving instance ToJSON InterlinearBlock

    getQueryR :: T.Text -> Handler TypedContent
    getQueryR q = do
        ShoeWeb shoeDB <- getYesod
        let rval = getGloss q shoeDB
        selectRep $ do
            provideRep $ return
                [shamlet|
                    <p>The french interlinearisation of #{q} is #{show rval}
                |]
            provideJson rval


    -- serve static files
    getScriptR f = getFile f ""
    getImagesR f = getFile f "images"

    -- home
    getHomeR = defaultLayout $ do
        queryWidget