{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.OgcSpec (main, spec) where

import           Network.Wai.Ogc

import           Data.Monoid ((<>))
import           Data.String (IsString, fromString)
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
  "/?SERVICE" `getFailsWith` EmptyParameterError "SERVICE"
  "/?SERVICE=" `getFailsWith` EmptyParameterError "SERVICE"
  "/?SeRViCe" `getFailsWith` EmptyParameterError "SERVICE"
  "/?SeRViCe=foo" `getFailsWith` InvalidParameterError "SERVICE" "foo"

wmsUrl :: (IsString a, Monoid a) => a -> a
wmsUrl = ("/?SERVICE=WMS&" <>)

wmsSpec :: SpecWith Application
wmsSpec = describe "WMS" $ do
  describe "missing mandatory parameters" $ do
    "/?SERVICE=WMS" `getFailsWith` MissingParameterError "REQUEST"
  wmsCapabilitiesSpec
  wmsMapSpec

wmsCapabilitiesSpec :: SpecWith Application
wmsCapabilitiesSpec = describe "GetCapabilities" $ do
  describe "with no optional parameters" $ do
    wmsUrl "REQUEST=GetCapabilities"
      `getSucceedsWith` wmsCapabilitiesRequest
  describe "legacy 1.0.0 request without explicit version" $ do
    wmsUrl "REQUEST=capabilities"
      `getSucceedsWith` wmsCapabilitiesRequest
  describe "with extra unknown query parameters" $ do
    wmsUrl "REQUEST=GetCapabilities&USER=foo"
      `getSucceedsWith` wmsCapabilitiesRequest
  describe "with VERSION parameter" $ do
    wmsUrl "REQUEST=GetCapabilities&VERSION=1.3.0"
      `getSucceedsWith` wmsCapabilitiesRequest { wmsCapVersion = Just Wms130 }
    wmsUrl "REQUEST=GetCapabilities&VERSION=1.3"
      `getSucceedsWith` wmsCapabilitiesRequest { wmsCapVersion = Just Wms130 }
    wmsUrl "REQUEST=GetCapabilities&VERSION=1.1.1"
      `getSucceedsWith` wmsCapabilitiesRequest { wmsCapVersion = Just Wms111 }
    wmsUrl "REQUEST=GetCapabilities&VERSION"
      `getFailsWith` EmptyParameterError "VERSION"
    describe "version 1.1.1 and 1.3.0 should send \"GetCapabilities\"\
      \ instead of \"capabilites\"" $ do
      wmsUrl "REQUEST=capabilities&VERSION=1.3.0"
        `getFailsWith` InvalidParameterError "REQUEST" "capabilities"
      wmsUrl "REQUEST=capabilities&VERSION=1.1.1"
        `getFailsWith` InvalidParameterError "REQUEST" "capabilities"
  describe "with legacy WMSTVER parameter" $ do
    wmsUrl "REQUEST=GetCapabilities&WMSTVER=1.0.0"
      `getFailsWith` InvalidParameterError "REQUEST" "GetCapabilities"
    wmsUrl "REQUEST=capabilities&WMSTVER=1.0.0"
      `getSucceedsWith` wmsCapabilitiesRequest { wmsCapVersion = Just Wms100 }
    wmsUrl "REQUEST=capabilities&WMSTVER=1.0"
      `getSucceedsWith` wmsCapabilitiesRequest { wmsCapVersion = Just Wms100 }
    wmsUrl "REQUEST=capabilities&WMSTVER=1"
      `getSucceedsWith` wmsCapabilitiesRequest { wmsCapVersion = Just Wms100 }
    describe "version 1 should send \"capabilites\"\
      \ instead of \"GetCapabilities\"" $ do
      wmsUrl "REQUEST=GetCapabilities&WMSTVER=1.0.0"
        `getFailsWith` InvalidParameterError "REQUEST" "GetCapabilities"
  describe "with FORMAT parameter" $ do
    wmsUrl "REQUEST=GetCapabilities&FORMAT=text/xml"
      `getSucceedsWith`
      wmsCapabilitiesRequest { wmsCapFormat = Just (Type (Text "xml") []) }
    wmsUrl "REQUEST=GetCapabilities&FORMAT=text--"
      `getFailsWith` InvalidParameterError "FORMAT" "text--"
  describe "with UPDATESEQUENCE parameter" $ do
    wmsUrl "REQUEST=GetCapabilities&UPDATESEQUENCE=34"
      `getSucceedsWith`
      wmsCapabilitiesRequest { wmsCapUpdateSequence = Just "34" }
    wmsUrl "REQUEST=GetCapabilities&UPDATESEQUENCE"
      `getFailsWith` EmptyParameterError "UPDATESEQUENCE"

wmsMapSpec :: SpecWith Application
wmsMapSpec = describe "GetMap" $ do
  let getMap    = wmsUrl . ("REQUEST=GetMap&" <>)
      getMap100 = wmsUrl . ("REQUEST=map&WMSTVER=1.0.0&" <>)
      getMap111 = getMap . ("VERSION=1.1.1&" <>)
      getMap130 = getMap . ("VERSION=1.3.0&" <>)
      epsg23030 = maybe (error "should not happen") id (epsgCrs 23030)
      pngFormat = Type (Image "png") []
      mapReq    = wmsMapRequest
        Wms130 [("foo","foo")] epsg23030 (Bbox 0 0 1 1) 1 2 pngFormat
      getMap130Min
        = getMap130
        . ("LAYERS=foo&STYLES=foo&CRS=EPSG:23030&BBOX=0,0,1,1\
           \&WIDTH=1&HEIGHT=2&FORMAT=image/png&" <>)

  describe "with missing mandatory parameters" $ do
    wmsUrl "REQUEST=GetMap" `getFailsWith` MissingParameterError "VERSION"
    getMap130 "" `getFailsWith` MissingParameterError "LAYERS"
    getMap130 "STYLES=foo" `getFailsWith` MissingParameterError "LAYERS"
    getMap130 "LAYERS=foo" `getFailsWith` MissingParameterError "STYLES"
    getMap130 "LAYERS=foo&STYLES=foo" `getFailsWith` MissingParameterError "CRS"
    getMap111 "LAYERS=foo&STYLES=foo" `getFailsWith` MissingParameterError "SRS"
    getMap100 "LAYERS=foo&STYLES=foo" `getFailsWith` MissingParameterError "SRS"
    getMap130 "LAYERS=foo&STYLES=foo&CRS=EPSG:23030"
      `getFailsWith` MissingParameterError "BBOX"
    getMap130 "LAYERS=foo&STYLES=foo&CRS=EPSG:23030&BBOX=0,0,1,1"
      `getFailsWith` MissingParameterError "WIDTH"
    getMap130 "LAYERS=foo&STYLES=foo&CRS=EPSG:23030&BBOX=0,0,1,1&WIDTH=1"
      `getFailsWith` MissingParameterError "HEIGHT"
    getMap130
      "LAYERS=foo&STYLES=foo&CRS=EPSG:23030&BBOX=0,0,1,1&WIDTH=1&HEIGHT=1"
      `getFailsWith` MissingParameterError "FORMAT"

  describe "with no optional parameters" $ do
    getMap130Min "" `getSucceedsWith` mapReq

  describe "with layers and styles length mismatch" $ do
    getMap130 "LAYERS=foo,bar&STYLES=foo"
      `getFailsWith` LayersStylesLengthMismatchError



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
