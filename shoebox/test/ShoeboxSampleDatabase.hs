{-# LANGUAGE OverloadedStrings #-}
module ShoeboxSampleDatabase where

import qualified Data.Map as M
import           Shoebox.Data

shoeDB :: ShoeDB
shoeDB = (shoeLexiconDB, shoeSuffixDB, shoePrefixDB, shoeSegmentationDB)

shoeLexiconDB :: ShoeLexiconDB
shoeLexiconDB = M.fromList
  [ ("maison",["house", "building"])
  , ("lorsque",["when"])
  , ("avoir",["to_have"])
  , ("six",["six"])
  , ("an",["year"])
  ]

shoeSuffixDB :: ShoeSuffixDB
shoeSuffixDB = M.fromList
  [ ("s",["PL"])
  , ("ais",["1sIPF"])
  ]

shoePrefixDB :: ShoePrefixDB
shoePrefixDB = M.fromList
  [ ("je",["1sPRON"])
  ]

shoeSegmentationDB :: ShoeSegmentationDB
shoeSegmentationDB = M.fromList
   [ ("maisons", [ MB [ MorphemeLex "maison"
                      , MorphemeSuffix "s"
                      ]
                 ]
     )
   , ("cadeaux", [ MB [ MorphemeLex "cadeau"
                      , MorphemeSuffix "s"
                      ]
                 ]
     )
   , ("j'avais", [ MB [ MorphemePrefix "je"
                      , MorphemeLex "avoir"
                      , MorphemeSuffix "ais"
                      ]
                 ]
     )
   , ("ans", [ MB [ MorphemeLex "an"
                  , MorphemeSuffix "s"
                  ]
             ]
     )
   ]

enAbbrevs :: ShoeDB
enAbbrevs = undefined
