{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module Network.Wai.Ogc.WmsSpec (main, spec) where

import           Network.Wai.Ogc.Internal.DurationSpec ()
import           Network.Wai.Ogc.Wms
import qualified Network.Wai.Ogc.Common as Common

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import           Data.Time (UTCTime(..))
import           Data.Time.Calendar (fromGregorian)
import qualified Data.List as L
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.String (fromString)
import           Network.HTTP.Types.URI (renderQuery, parseQuery)
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.Hspec

main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "missing mandatory parameters" $ do
    "/?SERVICE=WMS" `failsWith` MissingParameterError "REQUEST"

  wmsCapabilitiesSpec

  wmsMapSpec

  it "render/parse is idempotent" $
    property  $ \(req :: SomeRequest) ->
      uncurry parseRequest (renderRequest req) == Right req

  it "does not duplicate arguments" $
    property  $ \(req :: SomeRequest) ->
      let (query, _) = renderRequest req
          args = filter ((/="DIM_") . BS.take 4) (map fst query)
      in counterexample (show args) (length args == length (L.nub args))

{-
commonSpec :: Spec
commonSpec = describe "common" $ do
  "/" `failsWith` MissingParameterError "SERVICE"
  "/?SERVICE" `failsWith` EmptyParameterError "SERVICE"
  "/?SERVICE=" `failsWith` EmptyParameterError "SERVICE"
  "/?SeRViCe" `failsWith` EmptyParameterError "SERVICE"
  "/?SeRViCe=foo" `failsWith` InvalidParameterError "SERVICE" "foo"
-}

wmsCapabilitiesSpec :: Spec
wmsCapabilitiesSpec = describe "GetCapabilities" $ do

  renderParseSpec "with no optional parameters" wmsCapabilitiesRequest

  describe "legacy 1.0.0 request without explicit version" $ do
    "REQUEST=capabilities"
      `succeedsWith` wmsCapabilitiesRequest
  describe "with extra unknown query parameters" $ do
    "REQUEST=GetCapabilities&USER=foo"
      `succeedsWith` wmsCapabilitiesRequest
  describe "with VERSION parameter" $ do
    "REQUEST=GetCapabilities&VERSION=1.3.0"
      `succeedsWith` wmsCapabilitiesRequest { wmsCapVersion = Just Wms130 }
    "REQUEST=GetCapabilities&VERSION=1.3"
      `succeedsWith` wmsCapabilitiesRequest { wmsCapVersion = Just Wms130 }
    "REQUEST=GetCapabilities&VERSION=1.1.1"
      `succeedsWith` wmsCapabilitiesRequest { wmsCapVersion = Just Wms111 }
    "REQUEST=GetCapabilities&VERSION"
      `failsWith` EmptyParameterError "VERSION"
    describe "version 1.1.1 and 1.3.0 should send \"GetCapabilities\"\
      \ instead of \"capabilites\"" $ do
      "REQUEST=capabilities&VERSION=1.3.0"
        `failsWith` InvalidParameterError "REQUEST" "capabilities"
      "REQUEST=capabilities&VERSION=1.1.1"
        `failsWith` InvalidParameterError "REQUEST" "capabilities"
  describe "with legacy WMSTVER parameter" $ do
    "REQUEST=GetCapabilities&WMSTVER=1.0.0"
      `failsWith` InvalidParameterError "REQUEST" "GetCapabilities"
    "REQUEST=capabilities&WMSTVER=1.0.0"
      `succeedsWith` wmsCapabilitiesRequest { wmsCapVersion = Just Wms100 }
    "REQUEST=capabilities&WMSTVER=1.0"
      `succeedsWith` wmsCapabilitiesRequest { wmsCapVersion = Just Wms100 }
    "REQUEST=capabilities&WMSTVER=1"
      `succeedsWith` wmsCapabilitiesRequest { wmsCapVersion = Just Wms100 }
    describe "version 1 should send \"capabilites\"\
      \ instead of \"GetCapabilities\"" $ do
      "REQUEST=GetCapabilities&WMSTVER=1.0.0"
        `failsWith` InvalidParameterError "REQUEST" "GetCapabilities"
  describe "with FORMAT parameter" $ do
    "REQUEST=GetCapabilities&FORMAT=text/xml"
      `succeedsWith`
      wmsCapabilitiesRequest { wmsCapFormat = Just (Type (Text "xml") []) }
    "REQUEST=GetCapabilities&FORMAT=text--"
      `failsWith` InvalidParameterError "FORMAT" "text--"
  describe "with UPDATESEQUENCE parameter" $ do
    "REQUEST=GetCapabilities&UPDATESEQUENCE=34"
      `succeedsWith`
      wmsCapabilitiesRequest { wmsCapUpdateSequence = Just "34" }
    "REQUEST=GetCapabilities&UPDATESEQUENCE"
      `failsWith` EmptyParameterError "UPDATESEQUENCE"

wmsMapSpec :: Spec
wmsMapSpec = describe "GetMap" $ do
  let getMap    = ("REQUEST=GetMap&" <>)
      getMap100 = ("REQUEST=map&WMSTVER=1.0.0&" <>)
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
    "REQUEST=GetMap" `failsWith` MissingParameterError "VERSION"
    getMap130 "" `failsWith` MissingParameterError "LAYERS"
    getMap130 "STYLES=foo" `failsWith` MissingParameterError "LAYERS"
    getMap130 "LAYERS=foo" `failsWith` MissingParameterError "STYLES"
    getMap130 "LAYERS=foo&STYLES=foo" `failsWith` MissingParameterError "CRS"
    getMap111 "LAYERS=foo&STYLES=foo" `failsWith` MissingParameterError "SRS"
    getMap100 "LAYERS=foo&STYLES=foo" `failsWith` MissingParameterError "SRS"
    getMap130 "LAYERS=foo&STYLES=foo&CRS=EPSG:23030"
      `failsWith` MissingParameterError "BBOX"
    getMap130 "LAYERS=foo&STYLES=foo&CRS=EPSG:23030&BBOX=0,0,1,1"
      `failsWith` MissingParameterError "WIDTH"
    getMap130 "LAYERS=foo&STYLES=foo&CRS=EPSG:23030&BBOX=0,0,1,1&WIDTH=1"
      `failsWith` MissingParameterError "HEIGHT"
    getMap130
      "LAYERS=foo&STYLES=foo&CRS=EPSG:23030&BBOX=0,0,1,1&WIDTH=1&HEIGHT=1"
      `failsWith` MissingParameterError "FORMAT"

  renderParseSpec "with no optional parameters" mapReq

  renderParseSpec "with unicode layer names"
    mapReq { wmsMapLayers = [mkLayer' "Avión" ""] }

  renderParseSpec "with two layers with default styles"
    mapReq { wmsMapLayers = [mkLayer' "Avión" "", mkLayer' "Camión" ""] }

  describe "with TIME" $ do
    (renderRequestQS mapReq <> "&TIME=2000-07-01/2000-07-31/P1D")
      `succeedsWith` mapReq {
          wmsMapTime = Just $
            Interval
              (TimeStamp (UTCTime (fromGregorian 2000 7 1) 0))
              (TimeStamp (UTCTime (fromGregorian 2000 7 31) 0))
              (Just (DurationDate (DurDateDay (DurDay 1) Nothing)))
          }

  describe "miscelaneous invalid requests" $ do
    getMap130 "LAYERS=foo,bar&STYLES=foo"
      `failsWith` LayersStylesLengthMismatchError
    getMap130
      "LAYERS=f&STYLES=f&CRS=EPSG:1&BBOX=0,0,0,1&WIDTH=1&HEIGHT=1&FORMAT=image/png"
      `failsWith` InvalidParameterError "BBOX" "0,0,0,1"
    getMap130
      "LAYERS=f&STYLES=f&CRS=EPSG:1&BBOX=0,0,1,1&WIDTH=0&HEIGHT=1&FORMAT=image/png"
      `failsWith` InvalidParameterError "WIDTH" "0"
    getMap130
      "LAYERS=f&STYLES=f&CRS=EPSG:1&BBOX=0,0,1,1&WIDTH=-1&HEIGHT=1&FORMAT=image/png"
      `failsWith` InvalidParameterError "WIDTH" "-1"

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

renderRequestQS :: Common.Request a => a -> String
renderRequestQS = BS.unpack . renderRequestQS'

renderRequestQS' :: Common.Request a => a -> ByteString
renderRequestQS' = renderQuery True . fst . renderRequest

renderParseSpec :: (Show a, Eq a, Common.Request a) => String -> a -> Spec
renderParseSpec name req =
  it name (uncurry parseRequest (renderRequest req) == Right req)


failsWith :: String -> ParseError -> Spec
failsWith url err =
  describe ("GET " ++ url) $
    it ("fails with " ++ show err) $
      parseRequest (parseQuery (BS.pack url)) Nothing == (Left err :: Either ParseError SomeRequest)

succeedsWith :: (Show a, Eq a, Common.Request a) => String -> a -> Spec
succeedsWith url req =
  describe ("GET " ++ url) $
    it ("succeeds with " ++ show req) $
      parseRequest (parseQuery (BS.pack url)) Nothing == Right req


instance Arbitrary SomeRequest where
  arbitrary = oneof [
      SomeRequest <$> (arbitrary  :: Gen (Request Capabilities))
    , SomeRequest <$> (arbitrary  :: Gen (Request Map))
    , SomeRequest <$> (arbitrary  :: Gen (Request FeatureInfo))
    ]


instance Arbitrary (Request Capabilities) where
  arbitrary = GetCapabilities <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (Request Map) where
  arbitrary = GetMap
    <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    <*> arbitrary <*> arbitrary

instance Arbitrary (Request FeatureInfo) where
  arbitrary = GetFeatureInfo
    <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    <*> (fmap getPositive <$> arbitrary)

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

instance {-# OVERLAPS #-} Arbitrary [Name] where
  arbitrary = listOf1 arbitrary

instance {-# OVERLAPS #-}  Arbitrary [Layer] where
  arbitrary = listOf1 arbitrary

instance Arbitrary Layer where
  arbitrary = maybe arbitrary return =<< mkLayer <$> arbitrary <*> arbitrary

instance Arbitrary Name where
  arbitrary = maybe arbitrary return =<< mkName <$> arbitrary

instance Arbitrary Dimension where
  arbitrary = Dimension <$> (T.toUpper <$> arbitrary)
                        <*> (fromString <$> listOf1 arbitrary)


instance Arbitrary Time where
  arbitrary = oneof [ Interval <$> arbitrary <*> arbitrary <*> arbitrary
                    , Time     <$> arbitrary
                    ]

instance Arbitrary TimeStamp where
  arbitrary = oneof [pure Current, TimeStamp <$> arbitrary]

instance Arbitrary UpdateSequence where
  arbitrary = fromString <$> listOf1 arbitrary

instance Arbitrary Exceptions where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary Transparent where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary Version where
  arbitrary = elements [minBound..maxBound]

instance Arbitrary BgColor where
  arbitrary = BgColor <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Size where
  arbitrary = mkSize' <$> (getPositive <$> arbitrary)
                      <*> (getPositive <$> arbitrary)

instance Arbitrary Pixel where
  arbitrary = Pixel <$> arbitrary <*> arbitrary

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
