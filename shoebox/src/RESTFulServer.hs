{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Char (toLower)
import Data.Int
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Types (status200, status401, status404, status500)
import Network.HTTP.Types.Header (hContentType, hCacheControl)
import Network.Wai
import Network.Wai.Handler.Warp

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T

data ExampleRequest = ExampleRequest {
      erName        :: Text
    , erAge         :: Int32
    } deriving (Generic, Show, Eq)

instance FromJSON ExampleRequest where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = map toLower . drop 2 }

data ExampleResponse = ExampleResponse {
      exrName       :: Text
    , exrMessage    :: Text
    } deriving (Generic, Show, Eq)

instance ToJSON ExampleResponse where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = map toLower . drop 3 }

serve :: Application
serve req f = do
    body <- L.take 65536 <$> lazyRequestBody req
    case pathInfo req of
        ["example"] -> do
            let exampleRequest = decode body :: Maybe ExampleRequest
            case exampleRequest of
                Nothing ->
                    f $ responseLBS status500 [(hContentType, "text/json")] "UnableToParseJSON"
                Just exampleRequest' -> do
                    let response = ExampleResponse (erName exampleRequest') message
                        message = if (erAge exampleRequest' >= 18)
                            then "You're old enough"
                            else "Hey, you're too young"
                    f $ responseLBS status200 [(hContentType, "text/json")] $ encode response

        _ -> f $ responseLBS status200 [(hContentType, "text/json")] $ "UnknownAPICall"


main :: IO ()
main = print "Listening" >> run 3000 serve
