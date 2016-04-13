{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.OgcSpec (main, spec) where

import           Network.Wai.Ogc

import           Data.String (fromString)
import           Network.Wai (Application, responseLBS)
import           Network.HTTP.Types (status200, status400)
import           Test.Hspec
import           Test.Hspec.Wai

main :: IO ()
main = hspec spec


app :: Application
app req respond = do
  respond $ responseLBS
      status
      [("Content-Type", "text/plain")]
      (fromString body)
  where
    (body, status) = case parseRequest req of
      Right r -> (show r, status200)
      Left  e -> (show e, status400)

spec :: Spec
spec = with (return app) $ do
  commonSpec
  wmsSpec

commonSpec :: SpecWith Application
commonSpec = describe "common" $ do
  "/" `getFailsWith` MissingParameterError "SERVICE"
  "/?SERVICE" `getFailsWith` UnknownServiceError ""
  "/?SERVICE=" `getFailsWith` UnknownServiceError ""
  "/?SeRViCe" `getFailsWith` UnknownServiceError ""
  "/?SeRViCe=foo" `getFailsWith` UnknownServiceError "foo"

wmsSpec :: SpecWith Application
wmsSpec = describe "WMS" $ do
  describe "missing mandatory parameters" $ do
    "/?SERVICE=WMS" `getFailsWith` MissingParameterError "REQUEST"
  describe "GetCapabilities" $ do
    describe "with no optional parameters" $ do
      "/?SERVICE=WMS&REQUEST=GetCapabilities"
        `getSucceedsWith` wmsCapabilitiesRequest
    describe "with VERSION parameter" $ do
      "/?SERVICE=WMS&REQUEST=GetCapabilities&VERSION=1.1.1"
        `getSucceedsWith` wmsCapabilitiesRequest { version = Just Wms111 }
      "/?SERVICE=WMS&REQUEST=GetCapabilities&VERSION=1.3.0"
        `getSucceedsWith` wmsCapabilitiesRequest { version = Just Wms130 }
      "/?SERVICE=WMS&REQUEST=GetCapabilities&VERSION=1.0.0"
        `getFailsWith` UnknownVersionError WMS "1.0.0"
      "/?SERVICE=WMS&REQUEST=GetCapabilities&VERSION"
        `getFailsWith` UnknownVersionError WMS ""
    describe "with FORMAT parameter" $ do
      "/?SERVICE=WMS&REQUEST=GetCapabilities&FORMAT=text/xml"
        `getSucceedsWith`
        wmsCapabilitiesRequest { format = Just (Type (Text "xml") []) }
      "/?SERVICE=WMS&REQUEST=GetCapabilities&FORMAT=text--"
        `getFailsWith` InvalidMimeError "text--"
    describe "with UPDATESEQUENCE parameter" $ do
      "/?SERVICE=WMS&REQUEST=GetCapabilities&UPDATESEQUENCE=34"
        `getSucceedsWith` wmsCapabilitiesRequest { updateSequence = Just "34" }
      "/?SERVICE=WMS&REQUEST=GetCapabilities&UPDATESEQUENCE"
        `getFailsWith` EmptyUpdateSequenceError

getFailsWith :: String -> OgcRequestError -> SpecWith Application
getFailsWith url err =
  describe ("GET " ++ url) $
    it ("fails with " ++ show err) $
      let body = fromString (show err)
      in get (fromString url) `shouldRespondWith` body {matchStatus = 400}

getSucceedsWith :: String -> OgcRequest -> SpecWith Application
getSucceedsWith url req =
  describe ("GET " ++ url) $
    it ("succeeds with " ++ show req) $
      let body = fromString (show req)
      in get (fromString url) `shouldRespondWith` body {matchStatus = 200}
