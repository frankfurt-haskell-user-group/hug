{-# LANGUAGE OverloadedStrings #-}
module ShoeboxSpec(main, spec) where

import Test.Hspec
import Shoebox.Data
import Shoebox.Basics
import ShoeboxSampleDatabase
import Data.Typeable
--import Data.UUID
--import Data.UUID.V4
--import System.Random

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Shoebox" $ do

  it "can break up a piece of text into morphemes" $ do
    breakTX "maisons" shoeSegmentationDB
      `shouldBe` [ MB [MorphemeLex "maison", MorphemeSuffix "s"] ]
    breakTX "cadeaux" shoeSegmentationDB
      `shouldBe` [ MB [MorphemeLex "cadeau", MorphemeSuffix "s"] ]

  it "can lookup the meanings of a morpheme break" $ do
    lookupMB (MB [MorphemeLex "maison", MorphemeSuffix "s"]) shoeLexiconDB shoeSuffixDB shoePrefixDB
      `shouldBe` [ MeaningChoice ["house", "building"], AbbreviationChoice ["PL"] ]

  it "can gloss a piece of text" $ do
    gloss "maisons" shoeDB
      `shouldBe` [ ILB (TX "maisons")
                       (MB [MorphemeLex "maison",MorphemeSuffix "s"])
                       (GL [MeaningChoice ["house","building"],AbbreviationChoice ["PL"]])
                 ]
    gloss "j'avais" shoeDB
      `shouldBe` [ ILB (TX "j'avais")
                       (MB [ MorphemePrefix "je"
                           , MorphemeLex "avoir"
                           , MorphemeSuffix "ais"
                           ])
                       (GL [ AbbreviationChoice ["1sPRON"]
                           , MeaningChoice ["to_have"]
                           , AbbreviationChoice ["1sIPF"]
                           ])
                 ]

  it "can gloss a sentence" $ do
    intl "lorsque j'avais six ans" shoeDB
      `shouldBe` [ [ ILB (TX "lorsque")
                         (MB [MorphemeLex "lorsque"])
                         (GL [MeaningChoice ["when"]])
                   ]
                 , [ ILB (TX "j'avais")
                         (MB [ MorphemePrefix "je"
                             , MorphemeLex "avoir"
                             , MorphemeSuffix "ais"])
                         (GL [ AbbreviationChoice ["1sPRON"]
                             , MeaningChoice ["to_have"]
                             , AbbreviationChoice ["1sIPF"]])
                   ]
                 , [ ILB (TX "six")
                         (MB [ MorphemeLex "six"])
                         (GL [ MeaningChoice ["six"]])
                   ]
                 , [ ILB (TX "ans")
                         (MB [ MorphemeLex "an"
                             , MorphemeSuffix "s"])
                         (GL [ MeaningChoice ["year"]
                             , AbbreviationChoice ["PL"]])
                   ]
                 ]

  it "can pretty print an interlinear block" $ do
    pp [ ILB (TX "maisons")
             (MB [MorphemeLex "maison",MorphemeSuffix "s"])
             (GL [MeaningChoice ["house"],AbbreviationChoice ["PL"]])
       ] `shouldBe` "maisons\nmaison-s\nhouse -PL\n"

  it "can strip off punctuation from a sentence" $ do
    removePunc enAbbrevs "This, however, is an «example» for removed punctuation-marks - ignoring in-word punctuation like in dates or abbreviations, e.g. 21.10.2015."
      `shouldBe` "This however is an example for removed punctuation-marks ignoring in-word punctuation like in dates or abbreviations e.g. 21.10.2015"

  -- it "can convert a single data set from a lexical database" $ do
  --   importLexDBElem "\\le abaisser\n\\_no 00001\n\\me senken; herabsetzen; herunterlassen; vermindern; entwürdigen; demütigen\n\\lk 4\n\\co \n\\dt 17/Aug/15"
  --     `shouldBe` ("abaisser", [ ME [ "senken"
  --                                  , "herabsetzen"
  --                                  , "herunterlassen"
  --                                  , "vermindern"
  --                                  , "entwürdigen"
  --                                  , "demütigen"
  --                                  ]
  --                             , LK ["4"]
  --                             , CO [""]
  --                             , DT ["17/Aug/15"]
  --                             ]
  --                )

  -- it "can convert a single data set from a suffix database" $ do
  --   importSuffDBElem "\\le a\n\\_no 00002\n\\me 3sPF; 3sFUT\n\\co"
  --     `shouldBe`
  --       ("a", [ ME [ "3sPF", "3sFUT" ]
  --             , CO [""]
  --             ]
  --       )

  -- it "can extract prefixes from a segmentation (former \"parsing\") database" $ do
  --   importPrefixDBElem shoeSuffixDB "\\fl j'avais\n\\_no 03218\n\\bk je-avoir-ais"
  --     `shouldBe`
  --       ("je", [ ME [ "1sPRON" ]
  --              , CO [""]
  --              ]
  --       )

  -- it "can convert a single data set from a segmentation database" $ do
  --   importSegmentationDBElem shoeSuffixDB shoePrefixDB "\\fl j'avais\n\\_no 03218\n\\bk je-avoir-ais"
  --     `shouldBe`
  --       ("j'avais", [ MB [ MorphemePrefix "je"
  --                        , MorphemeLex "avoir"
  --                        , MorphemeSuffix "aiss"
  --                        ]
  --                   ]
  --       )

  --   importSegmentationDBElem shoeSuffixDB shoePrefixDB	"\\fl l'a\n\\_no 03306\n\\bk le-avoir-e; la-avoir-e"
  --     `shouldBe`
  --       ("j'avais", [ MB [ MorphemePrefix "le"
  --                        , MorphemeLex "avoir"
  --                        , MorphemeSuffix "e"
  --                        ]
  --                   , MB [ MorphemePrefix "la"
  --                        , MorphemeLex "avoir"
  --                        , MorphemeSuffix "e"
  --                        ]
  --                   ]
  --       )

