{-# LANGUAGE OverloadedStrings #-}
module Shoebox.Interface 
(

-- Database Management
ShoeDB,

shoeImportDB,
shoeListDBs,
shoeReadDB,
shoeWriteDB,
shoeDeleteDB,

-- Base Reading API
shoeQueryDB,

-- Base Writing API
shoeWriteSegDB,
shoeWriteLexDB,
shoeWritePrefixDB,
shoeWriteSuffixDB,
)
where

import Text.Printf
import Data.List (intercalate)
import Data.Text (Text, splitOn)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
import Data.Maybe

import System.Directory (getDirectoryContents, removeFile)
import System.FilePath (dropExtension, takeBaseName)
import Data.List.Split (splitOn)
import qualified Data.Map as M

import Control.Exception (catch, SomeException)
import qualified System.IO.Strict as S


import Shoebox.Data
import Shoebox.Basics
import Shoebox.Parser

-- this is the official interface towards the base functionality of the shoebox backend

-- DB Management
-- -------------
-- handle DB's, import, export, save, rename, ...

-- data directory is named "data" in current dir of serving process

shoeDataDir :: String
shoeDataDir = "data"

-- API remarks
-- all db names are not filenames, but basename, without extension
-- during import fz and similar are appended
-- native format ".sbx" is appended (shoebox)

-- imports from old format
shoeImportDB :: T.Text -> IO ShoeDB
shoeImportDB file = loadShoeDB (T.unpack file)

-- list existing new format DB's
shoeListDBs :: IO [Text]
shoeListDBs = do
    files <- getDirectoryContents shoeDataDir
    let dbs = map (T.pack . removeExt) (filter isSbx files)
    return dbs

-- read new format DB
shoeReadDB :: Text -> IO (Maybe ShoeDB)
shoeReadDB fname' = do
    let fname = (T.unpack fname') ++ ".sbx"
    catchAny (do
          readData <- S.readFile $ shoeDataDir ++ "/" ++ fname
          return (Just (read readData))
                ) $ \e -> return Nothing

-- save new format DB, existing data will be overwritten
shoeWriteDB :: ShoeDB -> Text -> IO ()
shoeWriteDB db fname' = do
    let fname = (T.unpack fname') ++ ".sbx"
    catchAny (do
          writeFile (shoeDataDir ++ "/" ++ fname) (show db)
          return ()) $ \e -> return ()

-- delete new format DB
shoeDeleteDB :: Text -> IO ()
shoeDeleteDB fname' = do
    let fname = (T.unpack fname') ++ ".sbx"
    catchAny (do
          removeFile (shoeDataDir ++ "/" ++ fname)
          return ()) $ \e -> return ()

-- helper, data files
-- ------------------

isSbx :: String -> Bool
isSbx name = case reverse (Data.List.Split.splitOn "." name) of 
    (x:xs) -> x == "sbx"
    _ -> False

removeExt :: String -> String
removeExt f = let
  s = (Data.List.Split.splitOn "." f)
  in case length s of
    1 -> s !! 0
    _ -> (s !! ((length s) -2))

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

-- Base Reading API for data
-- -------------------------

-- returns entries for input word from segDB, lexDB, prefixDB, suffixDB
shoeQueryDB :: ShoeDB -> Text -> ([MorphemeBreak], [Text], [Text], [Text])
shoeQueryDB shoeDB word = let
  (lexDB, suffDB, preDB, segDB) = shoeDB
  queryDB db = case M.lookup word db of 
      Nothing -> []
      Just l -> l
  in (queryDB segDB, queryDB lexDB, queryDB preDB, queryDB suffDB)

-- Base Writing API for data
-- -------------------------

writeDB :: M.Map Text [a] -> Text -> [a] -> M.Map Text [a]
writeDB mapDB key l = case l of
  [] -> M.delete key mapDB
  (x:xs) -> M.insert key l mapDB

-- write data into Segmentation DB, empty list clears key from Map
shoeWriteSegDB :: ShoeDB -> Text -> [MorphemeBreak] -> ShoeDB
shoeWriteSegDB db  key l = let
  (lexDB, suffDB, preDB, segDB) = db
  segDB' = writeDB segDB key l
  in (lexDB, suffDB, preDB, segDB')

-- write data into Lex DB, empty list clears key from Map
shoeWriteLexDB :: ShoeDB -> Text -> [Text] -> ShoeDB
shoeWriteLexDB db  key l = let
  (lexDB, suffDB, preDB, segDB) = db
  lexDB' = writeDB lexDB key l
  in (lexDB', suffDB, preDB, segDB)

-- write data into Suffix DB, empty list clears key from Map
shoeWriteSuffixDB :: ShoeDB -> Text -> [Text] -> ShoeDB
shoeWriteSuffixDB db  key l = let
  (lexDB, suffDB, preDB, segDB) = db
  suffDB' = writeDB suffDB key l
  in (lexDB, suffDB', preDB, segDB)

-- write data into Prefix DB, empty list clears key from Map
shoeWritePrefixDB :: ShoeDB -> Text -> [Text] -> ShoeDB
shoeWritePrefixDB db  key l = let
  (lexDB, suffDB, preDB, segDB) = db
  preDB' = writeDB preDB key l
  in (lexDB, suffDB, preDB', segDB)

