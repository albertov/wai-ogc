{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Network.Wai.OgcSpec (main, spec) where

import           Network.Wai.Ogc.Internal.DurationSpec ()
import           Network.Wai.Ogc

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import           Data.Time (UTCTime(..))
import           Data.Time.Calendar (fromGregorian)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.String (IsString, fromString)
import           Network.Wai (Application, responseLBS)
import           Network.HTTP.Types (status200, status400)
import           Network.HTTP.Types.URI (renderSimpleQuery)
import           Test.QuickCheck (getPositive, getNonNegative)
import           Test.QuickCheck.Instances ()
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.QuickCheck

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

  it "can parse a rendered arbitrary GET OgcRequest" $
    property  $ \(GetReq req) ->
      let body = fromString (show req)
          url  = renderRequestQS' req
      in get url `shouldRespondWith` body {matchStatus=200}


commonSpec :: SpecWith Application
commonSpec = describe "common" $ do
  "/" `getFailsWith` MissingParameterError "SERVICE"
  "/?SERVICE" `getFailsWith` EmptyParameterError "SERVICE"
  "/?SERVICE=" `getFailsWith` EmptyParameterError "SERVICE"
  "/?SeRViCe" `getFailsWith` EmptyParameterError "SERVICE"
  "/?SeRViCe=foo" `getFailsWith` InvalidParameterError "SERVICE" "foo"

wmsUrl :: (IsString a, Monoid a) => a -> a
wmsUrl = ("?SERVICE=WMS&" <>)

wmsSpec :: SpecWith Application
wmsSpec = describe "WMS" $ do
  describe "missing mandatory parameters" $ do
    "/?SERVICE=WMS" `getFailsWith` MissingParameterError "REQUEST"
  wmsCapabilitiesSpec
  wmsMapSpec

wmsCapabilitiesSpec :: SpecWith Application
wmsCapabilitiesSpec = describe "GetCapabilities" $ do

  renderParseSpec "with no optional parameters" wmsCapabilitiesRequest

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
        Wms130
        pngFormat
        [mkLayer' "foo" "foo"]
        epsg23030
        (mkSize' 1 2)
        (mkBbox' 0 0 1 1)

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

  renderParseSpec "with no optional parameters" mapReq

  renderParseSpec "with unicode layer names"
    mapReq { wmsLayers = [mkLayer' "Avión" ""] }

  renderParseSpec "with two layers with default styles"
    mapReq { wmsLayers = [mkLayer' "Avión" "", mkLayer' "Camión" ""] }

  describe "with TIME" $ do
    (renderRequestQS mapReq <> "&TIME=2000-07-01/2000-07-31/P1D")
      `getSucceedsWith` mapReq {
          wmsTime = Just $
            Interval
              (TimeStamp (UTCTime (fromGregorian 2000 7 1) 0))
              (TimeStamp (UTCTime (fromGregorian 2000 7 31) 0))
              (Just (DurationDate (DurDateDay (DurDay 1) Nothing)))
          }

  describe "miscelaneous invalid requests" $ do
    getMap130 "LAYERS=foo,bar&STYLES=foo"
      `getFailsWith` LayersStylesLengthMismatchError
    getMap130
      "LAYERS=f&STYLES=f&CRS=EPSG:1&BBOX=0,0,0,1&WIDTH=1&HEIGHT=1&FORMAT=image/png"
      `getFailsWith` InvalidParameterError "BBOX" "0,0,0,1"
    getMap130
      "LAYERS=f&STYLES=f&CRS=EPSG:1&BBOX=0,0,1,1&WIDTH=0&HEIGHT=1&FORMAT=image/png"
      `getFailsWith` InvalidParameterError "WIDTH" "0"
    getMap130
      "LAYERS=f&STYLES=f&CRS=EPSG:1&BBOX=0,0,1,1&WIDTH=-1&HEIGHT=1&FORMAT=image/png"
      `getFailsWith` InvalidParameterError "WIDTH" "-1"

mkLayer' :: T.Text -> T.Text -> Layer
mkLayer' n = fromMaybe (error "mkLayer': Invalid layer") . mkLayer n

mkSize' :: Int -> Int -> Size
mkSize' w = fromMaybe (error "mkSize': Invalid Size") . mkSize w

mkBbox' :: Scientific -> Scientific -> Scientific -> Scientific -> Bbox
mkBbox' x0 y0 x1 =
  fromMaybe (error "mkBbox': Invalid Bbox") . mkBbox x0 y0 x1

--
-- * Utils
--

renderRequestQS :: OgcRequest -> String
renderRequestQS = BS.unpack . renderRequestQS'

renderRequestQS' :: OgcRequest -> ByteString
renderRequestQS' = renderSimpleQuery True . fst . renderRequest

renderParseSpec :: String -> OgcRequest -> SpecWith Application
renderParseSpec name req =
  describe name (renderRequestQS req `getSucceedsWith` req)

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

newtype GetReq = GetReq OgcRequest
  deriving (Eq, Show)

instance Arbitrary GetReq where
  arbitrary = GetReq <$> oneof [ wmsCap
                               , wmsReq (pure Nothing)
                               , wmsReq (Just <$> arbitrary)
                               ]
    where
      wmsCap = WmsCapabilitiesRequest <$> arbitrary
                                      <*> arbitrary
                                      <*> arbitrary
      wmsReq q = WmsRequest <$> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                            <*> q

instance Arbitrary WmsFeatureInfoQuery where
  arbitrary = WmsFeatureInfoQuery <$> arbitrary
                                  <*> arbitrary
                                  <*> arbitrary
                                  <*> arbitrary
instance Arbitrary Type where
  arbitrary = Type <$> arbitrary <*> pure []
    -- FIXME: parseMIMEType does funky things with encoding

instance Arbitrary MIMEParam where
  arbitrary = MIMEParam <$> arbitrary <*> arbitrary

instance Arbitrary MIMEType where
  arbitrary = elements [
      Image "png"
    , Image "jpeg"
    , Image "gif"
    , Text "xml"
    , Video "mpeg"
    -- FIXME: parseMIMEType does funky things with encoding
    ]

instance Arbitrary [Name] where
  arbitrary = listOf1 arbitrary

instance Arbitrary [Layer] where
  arbitrary = listOf1 arbitrary

instance Arbitrary Layer where
  arbitrary = maybe arbitrary return =<< mkLayer <$> arbitrary <*> arbitrary

instance Arbitrary Name where
  arbitrary = maybe arbitrary return =<< mkName <$> arbitrary

instance Arbitrary Dimension where
  arbitrary = Dimension <$> (T.toUpper <$> arbitrary)
                        <*> (fromString <$> listOf1 arbitrary)


instance Arbitrary Elevation where
  arbitrary = Elevation <$> arbitrary

instance Arbitrary Time where
  arbitrary = oneof [ Interval <$> arbitrary <*> arbitrary <*> arbitrary
                    , Time     <$> arbitrary
                    ]

instance Arbitrary TimeStamp where
  arbitrary = oneof [pure Current, TimeStamp <$> arbitrary]

instance Arbitrary UpdateSequence where
  arbitrary = fromString <$> listOf1 arbitrary

instance Arbitrary WmsMapExceptions where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary Transparent where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary WmsVersion where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary BgColor where
  arbitrary = BgColor <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Size where
  arbitrary = mkSize' <$> (getPositive <$> arbitrary)
                      <*> (getPositive <$> arbitrary)

instance Arbitrary Pixel where
  arbitrary = Pixel <$> arbitrary <*> arbitrary

instance Arbitrary FeatureCount where
  arbitrary = FeatureCount <$> (getPositive <$> arbitrary)

instance Arbitrary Bbox where
  arbitrary = do
    x0 <- arbitrary
    y0 <- arbitrary
    w <- getPositive <$> arbitrary
    h <- getPositive <$> arbitrary
    return (mkBbox' x0 y0 (x0 + w) (y0 + h))

instance Arbitrary Crs where
  arbitrary = oneof [named, coded "EPSG", coded "CRS", coded "SR-ORG"]
    where
      named      = namedCrs <$> listOf1 arbitrary
      coded name = maybe (coded name) return
               =<< (codedCrs name <$> (getNonNegative <$> arbitrary))
