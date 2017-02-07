{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric #-}
module Shoebox.Data where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Typeable
import Data.Data
import Data.Aeson
import GHC.Generics

-- The data types
-- --------------

-- The input (raw) data without any further processing is a TextLine
newtype TextLine = TX Text deriving (Show, Read, Eq, Data, Typeable, Generic)

-- A TextLine is broken up into Morpheme's, which can be lexical elements, suffixed or prefixes
newtype MorphemeBreak = MB [Morpheme] deriving (Show, Read, Eq, Data, Typeable, Generic)
data Morpheme = MorphemeLex Text | MorphemeSuffix Text | MorphemePrefix Text deriving (Show, Read, Eq, Data, Typeable, Generic)

-- A GlossLine summarizes all Glosses, which are meanings attached to Morphemes
newtype GlossLine = GL [Choice] deriving (Show, Read, Eq, Data, Typeable, Generic)
data Choice = MeaningChoice [Text] | AbbreviationChoice [Text] deriving (Show, Read, Eq, Data, Typeable, Generic)

-- An interlinear block is a "categorized" textline, inluding its morphemes and glosses
data InterlinearBlock = ILB TextLine MorphemeBreak GlossLine deriving (Show, Read, Eq, Data, Typeable, Generic)


-- The databases
-- -------------

type ShoeSegmentationDB = M.Map Text [MorphemeBreak] -- segmentation
type ShoeLexiconDB = M.Map Text [Text] -- base words, lexicon
type ShoeSuffixDB  = M.Map Text [Text] -- suffixes
type ShoePrefixDB  = M.Map Text [Text] -- prefixes
type ShoeDB = (ShoeLexiconDB, ShoeSuffixDB, ShoePrefixDB, ShoeSegmentationDB)

instance ToJSON MorphemeBreak
instance ToJSON Morpheme
instance ToJSON GlossLine
instance ToJSON Choice
instance ToJSON InterlinearBlock
instance ToJSON TextLine

instance FromJSON MorphemeBreak
instance FromJSON Morpheme
instance FromJSON GlossLine
instance FromJSON Choice
instance FromJSON InterlinearBlock
instance FromJSON TextLine

