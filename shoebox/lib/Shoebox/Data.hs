{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Shoebox.Data where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Typeable
import Data.Data

-- The data types
-- --------------

-- The input (raw) data without any further processing is a TextLine
newtype TextLine = TX Text deriving (Show, Read, Eq, Data, Typeable)

-- A TextLine is broken up into Morpheme's, which can be lexical elements, suffixed or prefixes
newtype MorphemeBreak = MB [Morpheme] deriving (Show, Read, Eq, Data, Typeable)
data Morpheme = MorphemeLex Text | MorphemeSuffix Text | MorphemePrefix Text deriving (Show, Read, Eq, Data, Typeable)

-- A GlossLine summarizes all Glosses, which are meanings attached to Morphemes
newtype GlossLine = GL [Choice] deriving (Show, Read, Eq, Data, Typeable)
data Choice = MeaningChoice [Text] | AbbreviationChoice [Text] deriving (Show, Read, Eq, Data, Typeable)

-- An interlinear block is a "categorized" textline, inluding its morphemes and glosses
data InterlinearBlock = ILB TextLine MorphemeBreak GlossLine deriving (Show, Read, Eq, Data, Typeable)


-- The databases
-- -------------

type ShoeSegmentationDB = M.Map Text [MorphemeBreak]  -- segmentation
type ShoeLexiconDB = M.Map Text [Text] -- base words, lexicon
type ShoeSuffixDB  = M.Map Text [Text] -- suffixes
type ShoePrefixDB  = M.Map Text [Text] -- prefixes
type ShoeDB = (ShoeLexiconDB, ShoeSuffixDB, ShoePrefixDB, ShoeSegmentationDB)

