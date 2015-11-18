{-# LANGUAGE OverloadedStrings #-}
module Shoebox where

import Text.Printf
import Data.List (intercalate)
import Data.Text (Text, splitOn)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Maybe

-- types of databases
type ShoeSegmentationDB = M.Map Text [MorphemeBreak]  -- segmentation
type ShoeLexiconDB = M.Map LexEl [Meaning]       -- base words, lexicon
type ShoeSuffixDB  = M.Map SuffixEl [Abbreviation] -- suffixes
type ShoePrefixDB  = M.Map PrefixEl [Abbreviation] -- prefixes

type SuffixEl = Text
type PrefixEl = Text
type Abbreviation = Text
type Meaning = Text
type LexEl  = Text  -- inside lexicon
type FullEl = Text -- full element
type Suffix = Text -- suffix
type Prefix = Text -- prefix
type TextEl = Text

newtype TextLine = TX TextEl
  deriving (Show, Eq)

newtype GlossLine = GL [Choice]
  deriving (Show, Eq)

data Choice = Choice [Gloss]
  deriving (Show,Eq)

type Database = Text

data Gloss = Gloss Database Text
  deriving (Eq)

instance Show Gloss where
  show (Gloss _ x) = "(Gloss " ++ T.unpack x ++ ")"

newtype MorphemeBreak = MB [Morpheme]
  deriving (Show,Eq)

data Morpheme = MorphemeLex LexEl | MorphemeSuffix Suffix | MorphemePrefix Prefix
  deriving (Show,Eq)

data InterlinearBlock = ILB TextLine MorphemeBreak GlossLine
  deriving (Show,Eq)

type ShoeDB = (ShoeLexiconDB, ShoeSuffixDB, ShoePrefixDB, ShoeSegmentationDB)

breakTX :: TextEl -> ShoeSegmentationDB -> [MorphemeBreak]
breakTX textEl segmentationDB =
  fromMaybe [MB [MorphemeLex textEl]]
    (M.lookup textEl segmentationDB)

lookupMB :: MorphemeBreak -> ShoeLexiconDB -> ShoeSuffixDB -> ShoePrefixDB -> [Choice]
lookupMB (MB mbs) lexiconDB suffixDB prefixDB = map go mbs
  where
    go (MorphemeLex l)    = Choice $ Gloss "lexiconDB" <$> fromMaybe [] (M.lookup l lexiconDB)
    go (MorphemeSuffix s) = Choice $ Gloss "suffixDB" <$> fromMaybe [] (M.lookup s suffixDB)
    go (MorphemePrefix p) = Choice $ Gloss "prefixDB" <$> fromMaybe [] (M.lookup p prefixDB)

data Decision = Abort | Decided Gloss | Skip
  deriving Show

decide :: Choice -> IO Decision
decide (Choice choices) = do
  printf "Multiple glosses found, please choose:\n%s\n"
             (showChoices choices)
  printf "(x) none of these, enter new gloss\n"
  printf "(s) skip this gloss\n"
  printf "(m) annotate manually\n"
  printf "(a) abort interlinearization\n"
  printf "(t) try applying external script\n"
  line <- getLine
  case readMaybe line :: Maybe Int of
    Nothing ->
      let action = head line
      in case action of
           'x' -> undefined
           's' -> return Skip
           'm' -> annotateManually
           'a' -> return Abort
           't' -> undefined
           _ -> do
             printf "unrecognized option, try again.\n"
             decide (Choice choices)
    Just n | n == 0 || n > length choices -> do
               printf "Choice out of range, try again.\n"
               decide (Choice choices)
           | otherwise ->
               return $ Decided $ choices !! (n-1)

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  []        -> Nothing
  ((x,_):_) -> Just x

annotateManually :: IO Decision
annotateManually = undefined

showChoices :: [Gloss] -> String
showChoices l = intercalate ", " $ do
  (n,Gloss _ g) <- zip [(1::Int)..] l
  return $ concat ["(", show n, ") ", T.unpack g]

gloss :: TextEl -> ShoeDB -> [InterlinearBlock]
gloss textEl (lexiconDB,suffixDB,prefixDB,segmentationDB) = do
  morphemeBreak <- breakTX textEl segmentationDB
  let glosses = GL (lookupMB morphemeBreak lexiconDB suffixDB prefixDB)
  return $ ILB (TX textEl) morphemeBreak glosses

intlx :: [TextEl] -> ShoeDB -> [[InterlinearBlock]]
intlx xs shoeDB =  map (`gloss` shoeDB) xs

intl :: Text -> ShoeDB -> [[InterlinearBlock]]
intl s = intlx (splitOn " " s)

pp :: [InterlinearBlock] -> Text
pp = undefined

removePunc :: ShoeDB -> Text -> Text
removePunc = undefined

importLexDBElem :: Text -> DBElem
importLexDBElem = undefined

importSuffDBElem :: Text -> DBElem
importSuffDBElem = undefined

importPrefixDBElem :: ShoeSuffixDB -> Text -> (Text, [DBElem])
importPrefixDBElem = undefined

importSegmentationDBElem :: ShoeSuffixDB -> ShoePrefixDB -> Text -> (Text, [DBElem])
importSegmentationDBElem = undefined

--genuuid :: IO Data.UUID.Types.Internal.UUID
--genuuid = nextRandom

--isUUID :: (Typeable a) => a -> Bool
--isUUID n = typeOf n == typeOf nextRandom


