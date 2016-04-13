{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Network.Wai.Ogc (
    OgcRequest       (..)
  , OgcRequestError  (..)
  , WmsVersion       (..)
  , WmsMapExceptions (..)
  , Service          (..)
  , Bbox             (..)
  , RgbColor         (..)
  , Transparent      (..)
  , Time             (..)
  , TimeInterval     (..)
  , TimeResolution   (..)
  , UpdateSequence

  , parseRequest

  , wmsCapabilitiesRequest
  , wmsMapRequest

    -- * Re-exports
  , MIME.Type (..)
  , MIME.MIMEType (..)
  , MIME.MIMEParam (..)
  , module SR
) where

import           Control.Applicative ((<|>))
import           Control.Arrow (first)
import qualified Codec.MIME.Parse as MIME
import qualified Codec.MIME.Type as MIME
import qualified Data.Attoparsec.ByteString.Char8 as AP
import           Data.ByteString (ByteString)
import           Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import qualified Data.List as L
import           Data.Scientific (Scientific)
import           Data.String (IsString)
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8With)
import           Data.Text.Encoding.Error (lenientDecode)
import           Data.Time (UTCTime)
import           Data.Typeable (Typeable)
import           Data.Word (Word8)
import           SpatialReference as SR
import           Network.Wai (Request, queryString)

--
-- * Public types and functions
--

type Layer = Text
type Style = Text

data Bbox =
  Bbox {
    minx :: !Scientific
  , miny :: !Scientific
  , maxx :: !Scientific
  , maxy :: !Scientific
  } deriving Show

data RgbColor =
  RgbColor {
    red   :: !Word8
  , green :: !Word8
  , blue  :: !Word8
  } deriving Show

data Transparent = Opaque | Transparent
  deriving (Show, Eq, Ord, Enum, Bounded)


data OgcRequest
  = WcsRequest
  | WfsRequest
  | WmsCapabilitiesRequest {
      wmsCapFormat         :: Maybe MIME.Type
    , wmsCapVersion        :: Maybe WmsVersion
    , wmsCapUpdateSequence :: Maybe UpdateSequence
    }
  | WmsMapRequest {
      wmsMapVersion          :: WmsVersion
    , wmsMapLayers           :: [(Layer, Style)]
    , wmsMapCrs              :: SR.Crs
    , wmsMapBbox             :: Bbox
    , wmsMapWidth            :: Int
    , wmsMapHeight           :: Int
    , wmsMapFormat           :: MIME.Type
    , wmsMapTransparent      :: Maybe Transparent
    , wmsMapBackground       :: Maybe RgbColor
    , wmsMapExceptions       :: Maybe WmsMapExceptions
    , wmsMapTime             :: Maybe (Either Time TimeInterval)
    , wmsMapElevation        :: Maybe Scientific
    , wmsMapDimensions       :: [Dimension]
    }
  | WpsRequest
  deriving Show

data Time = Time UTCTime | Current
  deriving Show

data TimeInterval =
  TimeInterval {
    tiStart      :: Time
  , tiEnd        :: Time
  , tiResolution :: Maybe TimeResolution
  } deriving Show

data TimeResolution =
  TimeResolution {
    resYears   :: Maybe Int
  , resMonths  :: Maybe Int
  , resDays    :: Maybe Int
  , resHours   :: Maybe Int
  , resMinutes :: Maybe Int
  , resSeconds :: Maybe Int
  } deriving Show


type DimensionName  = ByteString  -- ^ without the "DIM_" prefix
type DimensionValue = ByteString  -- ^ unparsed since it is application dependant
type Dimension      = (DimensionName, DimensionValue)

wmsCapabilitiesRequest :: OgcRequest
wmsCapabilitiesRequest = WmsCapabilitiesRequest Nothing Nothing Nothing

wmsMapRequest
  :: WmsVersion
  -> [(Layer, Style)]
  -> SR.Crs
  -> Bbox
  -> Int
  -> Int
  -> MIME.Type
  -> OgcRequest
wmsMapRequest version layers crs bbox width height format =
  WmsMapRequest {
    wmsMapVersion     = version
  , wmsMapLayers      = layers
  , wmsMapCrs         = crs
  , wmsMapBbox        = bbox
  , wmsMapWidth       = width
  , wmsMapHeight      = height
  , wmsMapFormat      = format
  , wmsMapTransparent = Nothing
  , wmsMapBackground  = Nothing
  , wmsMapExceptions  = Nothing
  , wmsMapTime        = Nothing
  , wmsMapElevation   = Nothing
  , wmsMapDimensions  = []
  }

data WmsVersion
  = Wms100
  | Wms111
  | Wms130
  deriving (Show, Eq, Ord, Enum, Bounded)

data OgcRequestError
  = MissingParameterError           ByteString
  | EmptyParameterError             ByteString
  | InvalidParameterError           ByteString ByteString
  | LayersStylesLengthMismatchError
  | NotImplementedError
  deriving (Show, Typeable)

data WmsMapExceptions = WmsMapExcXml | WmsMapExcInImage | WmsMapExcBlank
  deriving (Show, Eq, Ord, Enum, Bounded)

newtype UpdateSequence = UpdateSequence ByteString
  deriving (Eq, Ord, Show, IsString)

parseRequest :: Request -> Either OgcRequestError OgcRequest
parseRequest req = do
  service <- reqService query
  case service of
    WCS -> Left NotImplementedError
    WFS -> Left NotImplementedError
    WMS -> parseWmsRequest req
    WPS -> Left NotImplementedError
  where
    query = queryStringCI req


--
-- * Internal types and functions
--

data QueryRequest
  = GetCapabilities
  | GetMap
  | GetFeatureInfo
  deriving (Show, Typeable)

type QueryCI = [(CI ByteString, Maybe ByteString)]

data Service = WCS | WFS | WMS | WPS
  deriving Show

reqService :: QueryCI -> Either OgcRequestError Service
reqService = mandatoryParameter "SERVICE" $
  AP.choice [
    AP.stringCI "WCS" *> pure WCS
  , AP.stringCI "WFS" *> pure WFS
  , AP.stringCI "WMS" *> pure WMS
  , AP.stringCI "WPS" *> pure WPS
  ]

parseWmsRequest :: Request -> Either OgcRequestError OgcRequest
parseWmsRequest req = do
  version <- reqWmsVersion
  request <- reqWmsRequest version
  case request of
    GetCapabilities -> parseWmsCapabilitiesRequest version
    GetMap          -> parseWmsMapRequest version
    GetFeatureInfo  -> undefined
  where
    query = queryStringCI req

    parseWmsCapabilitiesRequest wmsCapVersion = do
      wmsCapFormat <- reqFormat query
      wmsCapUpdateSequence <- reqUpdateSequence query
      return WmsCapabilitiesRequest {..}

    parseWmsMapRequest Nothing = Left (MissingParameterError "VERSION")
    parseWmsMapRequest (Just wmsMapVersion) = do
      wmsMapLayers <- reqLayers query
      let crsKey = if wmsMapVersion == Wms130 then "CRS" else "SRS"
      wmsMapCrs <- reqCrs crsKey query
      wmsMapBbox <- reqBbox query
      wmsMapWidth <- reqWidth query
      wmsMapHeight <- reqHeight query
      wmsMapFormat <- maybe (Left (MissingParameterError "FORMAT")) return
                  =<< reqFormat query
      wmsMapTransparent <- return Nothing -- FIXME
      wmsMapBackground <- return Nothing -- FIXME
      wmsMapExceptions <- return Nothing -- FIXME
      wmsMapTime <- return Nothing -- FIXME
      wmsMapElevation <- return Nothing -- FIXME
      wmsMapDimensions <- return [] -- FIXME
      return WmsMapRequest {..}

    reqWmsRequest version =
      mandatoryParameter "REQUEST"  parser query
      where
        parser = case version of
          Just Wms100 -> wms100RequestParser
          Just _      -> wmsRequestParser
          Nothing     -> wmsRequestParser <|> wms100RequestParser
        wms100RequestParser = AP.choice [
            AP.stringCI "capabilities" *> pure GetCapabilities
          , AP.stringCI "map"          *> pure GetMap
          , AP.stringCI "feature_info" *> pure GetFeatureInfo
          ]
        wmsRequestParser = AP.choice [
            AP.stringCI "GetCapabilities" *> pure GetCapabilities
          , AP.stringCI "GetMap"          *> pure GetMap
          , AP.stringCI "GetFeatureInfo"  *> pure GetFeatureInfo
          ]

    reqWmsVersion = maybe tryWmsTver (return . Just) =<< tryVersion
      where
        tryVersion = optionalParameter "VERSION"  parser query
          where
            parser = AP.choice [
                "1.1.1"             *> pure Wms111
              , ("1.3.0" <|> "1.3") *> pure Wms130
              ]
        tryWmsTver = optionalParameter "WMSTVER"  parser query
          where
            parser = ("1.0.0" <|> "1.0" <|> "1") *> pure Wms100

reqLayers :: QueryCI -> Either OgcRequestError [(Layer,Style)]
reqLayers query = do
  layers <- mandatoryParameter "LAYERS" parser query
  styles <- mandatoryParameter "STYLES" parser query
  if length layers == length styles
    then Right (zip layers styles)
    else Left LayersStylesLengthMismatchError
  where
    parser = (lenientDecodeUtf8 <$> AP.takeWhile1 (/=','))
              `AP.sepBy1` (AP.char ',')

reqCrs :: CI ByteString -> QueryCI -> Either OgcRequestError SR.Crs
reqCrs key = mandatoryParameter key (epsg <|> crs)
  where
    epsg, crs :: AP.Parser SR.Crs
    crs  = undefined
    epsg = maybe (fail "invalid epsg") return
       =<< (SR.epsgCrs <$> (AP.stringCI "EPSG:" *> AP.decimal))


reqBbox :: QueryCI -> Either OgcRequestError Bbox
reqBbox = mandatoryParameter "BBOX" parser
  where parser = Bbox <$> (AP.scientific <* AP.char ',')
                      <*> (AP.scientific <* AP.char ',')
                      <*> (AP.scientific <* AP.char ',')
                      <*> AP.scientific

reqWidth, reqHeight :: QueryCI -> Either OgcRequestError Int
reqWidth  = mandatoryParameter "WIDTH" posInt
reqHeight = mandatoryParameter "HEIGHT" posInt

reqFormat :: QueryCI -> Either OgcRequestError (Maybe MIME.Type)
reqFormat query =
  case L.lookup "FORMAT" query of
    Just (Just fmt)
      | Just mime <- parseMIME fmt -> Right (Just mime)
      | otherwise                  -> Left  (InvalidParameterError "FORMAT" fmt)
    Just Nothing                   -> Left  (EmptyParameterError "FORMAT")
    Nothing                        -> Right Nothing
  where
    parseMIME = MIME.parseMIMEType . lenientDecodeUtf8

reqUpdateSequence :: QueryCI -> Either OgcRequestError (Maybe UpdateSequence)
reqUpdateSequence = optionalParameter
  "UPDATESEQUENCE" (UpdateSequence <$> AP.takeWhile1 (const True))

--
-- * Utils
--

posInt :: AP.Parser Int
posInt = do
  n <- AP.decimal
  if n==0 then fail "Negative number" else return n

lenientDecodeUtf8 :: ByteString -> Text
lenientDecodeUtf8 = decodeUtf8With lenientDecode

mandatoryParameter
  :: CI ByteString
  -> AP.Parser a
  -> QueryCI
  -> Either OgcRequestError a
mandatoryParameter key parser
  = either Left (maybe (Left (MissingParameterError (CI.original key))) Right)
  . optionalParameter key parser

optionalParameter
  :: CI ByteString
  -> AP.Parser a
  -> QueryCI
  -> Either OgcRequestError (Maybe a)
optionalParameter key parser query =
  case L.lookup key query of
    Just (Just "") -> Left (EmptyParameterError (CI.original key))
    Just (Just s) ->
      either (const (Left (InvalidParameterError (CI.original key) s)))
             (Right . Just)
             (AP.parseOnly (parser <* AP.endOfInput) s)
    Just Nothing -> Left (EmptyParameterError (CI.original key))
    Nothing      -> Right Nothing


-- | Parses a query string into case-insensitive elements
queryStringCI :: Request -> QueryCI
queryStringCI = map (first CI.mk) . queryString
