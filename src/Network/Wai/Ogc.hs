{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Wai.Ogc (
    OgcRequest       (..)
  , OgcRequestError  (..)
  , WmsVersion       (..)
  , WmsMapExceptions (..)
  , Layer            (..)
  , Service          (..)
  , Bbox             (..)
  , Size             (..)
  , BgColor          (..)
  , Transparent      (..)
  , Time             (..)
  , TimeInterval     (..)
  , Elevation        (..)
  , Dimension        (..)
  , UpdateSequence

  , parseRequest
  , renderRequest

  , wmsCapabilitiesRequest
  , wmsMapRequest

    -- * Re-exports
  , MIME.Type (..)
  , MIME.MIMEType (..)
  , MIME.MIMEParam (..)
  , module SR
  , module Duration
) where

import           Network.Wai.Ogc.Internal.Duration as Duration

import           Control.Applicative ((<|>))
import           Control.Arrow (first)
import           Control.Monad (liftM)
import qualified Codec.MIME.Parse as MIME
import qualified Codec.MIME.Type as MIME
import           Data.Attoparsec.ByteString.Char8 as AP
import           Data.Bits (unsafeShiftR, (.&.))
import           Data.ByteString (ByteString)
import           Data.ByteString.Builder ( Builder
                                         , word8HexFixed
                                         , toLazyByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import qualified Data.List as L
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.Scientific (Scientific)
import           Data.String (IsString, fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import           Data.Time (UTCTime, defaultTimeLocale)
import           Data.Time.Format (formatTime, parseTimeM)
import           Data.Typeable (Typeable)
import           Data.Word (Word8, Word32)
import           SpatialReference as SR
import           Network.Wai (Request, queryString)
import           Network.HTTP.Types.URI (SimpleQuery)

--
-- * Public types and functions
--

data Layer =
  Layer {
    layerName  :: Text
  , layerStyle :: Text
  } deriving (Eq, Show)

instance FromQuery [Layer] () where
  fromQuery () query = do
    layers <- case mandatoryParameter "LAYERS" parser query of
          Left (EmptyParameterError _) -> Right [""]
          ls                           -> ls
    styles <- case (layers, mandatoryParameter "STYLES" parser query) of
          ([_], Left (EmptyParameterError _)) -> Right [""]
          (_ , ss)                            -> ss

    if length layers == length styles
      then Right (zipWith Layer layers styles)
      else Left LayersStylesLengthMismatchError
    where
      parser = (lenientDecodeUtf8 <$> AP.takeWhile (/=','))
                `sepBy1` (char ',')
  {-# INLINE fromQuery #-}

instance ToQueryItems [Layer] c where
  toQueryItems _ ls =
    [ ("LAYERS", BS.intercalate "," (map (T.encodeUtf8 . layerName) ls))
    , ("STYLES", BS.intercalate "," (map (T.encodeUtf8 . layerStyle) ls))
    ]
  {-# INLINE toQueryItems #-}

data Bbox =
  Bbox {
    minx :: !Scientific
  , miny :: !Scientific
  , maxx :: !Scientific
  , maxy :: !Scientific
  } deriving (Eq, Show)

instance FromQuery Bbox () where
  fromQuery () = mandatoryParameter "BBOX" parser
    where parser = Bbox <$> (scientific <* char ',')
                        <*> (scientific <* char ',')
                        <*> (scientific <* char ',')
                        <*> scientific
  {-# INLINE fromQuery #-}

instance ToQueryItems Bbox c where
  toQueryItems _ (Bbox a b c d) = [ ("BBOX", runBuilder bld)]
    where bld = mconcat (L.intersperse "," (map showScientific [a, b, c, d]))
  {-# INLINE toQueryItems #-}

--
-- * BgColor
--

data BgColor =
  BgColor {
    red   :: !Word8
  , green :: !Word8
  , blue  :: !Word8
  } deriving (Eq, Show)

instance FromQuery (Maybe BgColor) () where
  fromQuery () = optionalParameter "BGCOLOR" $ do
    hexVal :: Word32 <- "0x" *> hexadecimal
    let r = fromIntegral ((hexVal `unsafeShiftR` 16) .&. 0xFF)
        g = fromIntegral ((hexVal `unsafeShiftR` 8) .&. 0xFF)
        b = fromIntegral (hexVal .&. 0xFF)
    if hexVal <= 0xFFFFFF
       then return (BgColor r g b) else fail "Invalid RGB color"
  {-# INLINE fromQuery #-}

instance ToQueryItems BgColor c where
  toQueryItems _ (BgColor r g b) = [("BGCOLOR", runBuilder hexColor)]
    where
      hexColor = "0x" <> word8HexFixed r <> word8HexFixed g <> word8HexFixed b
  {-# INLINE toQueryItems #-}
--
-- * Transparent
--

data Transparent = Opaque | Transparent
  deriving (Show, Eq, Ord, Enum, Bounded)

instance FromQuery (Maybe Transparent) () where
  fromQuery () =
    optionalParameter "TRANSPARENT" $ choice
      [ stringCI "TRUE"  *> pure Transparent
      , stringCI "FALSE" *> pure Opaque]
  {-# INLINE fromQuery #-}

instance ToQueryItems Transparent c where
  toQueryItems _ Transparent = [("TRANSPARENT", "TRUE")]
  toQueryItems _ Opaque      = [("TRANSPARENT", "FALSE")]
  {-# INLINE toQueryItems #-}

data Size =
  Size {
    width  :: !Int
  , height :: !Int
  } deriving (Eq, Show)

instance FromQuery Size () where
  fromQuery () query =
    Size <$> mandatoryParameter "WIDTH" posInt query
         <*> mandatoryParameter "HEIGHT" posInt query
  {-# INLINE fromQuery #-}

instance ToQueryItems Size c where
  toQueryItems _ (Size w h) =
    [("WIDTH", fromString (show w)) , ("HEIGHT", fromString (show h))]
  {-# INLINE toQueryItems #-}


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
    , wmsMapLayers           :: [Layer]
    , wmsMapCrs              :: SR.Crs
    , wmsMapBbox             :: Bbox
    , wmsMapSize             :: Size
    , wmsMapFormat           :: MIME.Type
    , wmsMapTransparent      :: Maybe Transparent
    , wmsMapBackground       :: Maybe BgColor
    , wmsMapExceptions       :: Maybe WmsMapExceptions
    , wmsMapTime             :: Maybe (Either Time TimeInterval)
    , wmsMapElevation        :: Maybe Elevation
    , wmsMapDimensions       :: [Dimension]
    }
  | WpsRequest
  deriving (Eq, Show)

--
-- * Elevation
--

newtype Elevation = Elevation Scientific
  deriving (Eq, Ord, Show, Num)

instance FromQuery (Maybe Elevation) () where
  fromQuery () = optionalParameter "ELEVATION" (Elevation <$> scientific)
  {-# INLINE fromQuery #-}

instance ToQueryItems Elevation c where
  toQueryItems _ (Elevation e) = [("ELEVATION", showScientific e)]
  {-# INLINE toQueryItems #-}

--
-- * TimeInterval
--

data Time = Time UTCTime | Current
  deriving (Eq, Show)

data TimeInterval =
  TimeInterval {
    tiStart      :: Time
  , tiEnd        :: Time
  , tiResolution :: Maybe Duration
  } deriving (Eq, Show)

formatTime' :: IsString s => Time -> s
formatTime' Current  = "current"
formatTime' (Time t) = fromString (formatTime defaultTimeLocale "%FT%T%QZ" t)

instance FromQuery (Maybe (Either Time TimeInterval)) () where
  fromQuery () = optionalParameter "TIME"
    ((Right <$> parseInterval) <|> (Left  <$> parseTime))
    where
      parseInterval =
        TimeInterval
          <$> (parseTime <* char '/')
          <*> parseTime
          <*> ((endOfInput *> pure Nothing) <|> (Just <$> (char '/' *> duration)))
      parseTime = (stringCI "current" *> pure Current)
              <|> parseFmt "%FT%T%QZ"
              <|> parseFmt "%F"
      parseFmt fmt = liftM Time . parseTimeM False defaultTimeLocale fmt
                 =<< (BS.unpack <$> takeWhile1 (/='/'))
  {-# INLINE fromQuery #-}

instance ToQueryItems (Either Time TimeInterval) c where
  toQueryItems _ (Left t)  = [("TIME", formatTime' t)]
  toQueryItems _ (Right (TimeInterval s e Nothing)) =
    [("TIME", runBuilder (formatTime' s <> "/" <> formatTime' e))]
  toQueryItems _ (Right (TimeInterval s e (Just d))) = [("TIME", runBuilder b)]
    where b = formatTime' s <> "/" <> formatTime' e <> "/" <> formatDurationB d
  {-# INLINE toQueryItems #-}


--
-- * Dimension
--


data Dimension =
  Dimension {
    dimName  :: Text       -- ^ without the "DIM_" prefix, all-caps
  , dimValue :: ByteString -- ^ unparsed since it is application dependant
  } deriving (Eq, Show)


instance FromQuery [Dimension] () where
  fromQuery () = Right . foldr step []
    where
      step (splitted -> ("DIM_", k), Just v) z = (Dimension k v):z
      step _                                 z = z
      splitted = T.splitAt 4 . T.toUpper . lenientDecodeUtf8 . CI.original
  {-# INLINE fromQuery #-}



instance ToQueryItems [Dimension] c where
  toQueryItems _ =
    map (\(Dimension (T.encodeUtf8 . T.toUpper -> k) v) -> ("DIM_"<>k, v))
  {-# INLINE toQueryItems #-}


wmsCapabilitiesRequest :: OgcRequest
wmsCapabilitiesRequest = WmsCapabilitiesRequest Nothing Nothing Nothing

wmsMapRequest
  :: WmsVersion
  -> [Layer]
  -> SR.Crs
  -> Bbox
  -> Size
  -> MIME.Type
  -> OgcRequest
wmsMapRequest version layers crs bbox size format =
  WmsMapRequest {
    wmsMapVersion     = version
  , wmsMapLayers      = layers
  , wmsMapCrs         = crs
  , wmsMapBbox        = bbox
  , wmsMapSize        = size
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

instance FromQuery (Maybe WmsVersion) WmsVersion where
  fromQuery Wms100 = optionalParameter "WMSTVER"  parser
    where parser = ("1.0.0" <|> "1.0" <|> "1") *> pure Wms100
  fromQuery Wms111 = optionalParameter "VERSION"  parser
    where parser = "1.1.1" *> pure Wms111
  fromQuery Wms130 = optionalParameter "VERSION"  parser
    where parser = ("1.3.0" <|> "1.3") *> pure Wms130
  {-# INLINE fromQuery #-}

instance ToQueryItems WmsVersion WmsVersion where
  toQueryItems Wms100 Wms100 = [("WMSTVER", "1.0.0")]
  toQueryItems Wms111 Wms111 = [("VERSION", "1.1.1")]
  toQueryItems Wms130 Wms130 = [("VERSION", "1.3.0")]
  toQueryItems _      _      = error "ToQueryItems: WmsVersion"
  {-# INLINE toQueryItems #-}

data OgcRequestError
  = MissingParameterError           ByteString
  | EmptyParameterError             ByteString
  | InvalidParameterError           ByteString ByteString
  | LayersStylesLengthMismatchError
  | NotImplementedError
  deriving (Show, Typeable)

--
-- * WmsMapExceptions
--
data WmsMapExceptions = WmsMapExcXml | WmsMapExcInImage | WmsMapExcBlank
  deriving (Show, Eq, Ord, Enum, Bounded)

instance FromQuery (Maybe WmsMapExceptions) WmsVersion where
  fromQuery version = optionalParameter "EXCEPTIONS" parser
    where
      parser = case version of
                 Wms100 -> simpleParser
                 Wms130 -> simpleParser
                 Wms111 -> mimeParser
      simpleParser = choice [ stringCI "XML"     *> pure WmsMapExcXml
                            , stringCI "INIMAGE" *> pure WmsMapExcInImage
                            , stringCI "BLANK"   *> pure WmsMapExcBlank
                            ]
      mimeParser = stringCI "application/vnd.ogc_se" *> simpleParser
  {-# INLINE fromQuery #-}

instance ToQueryItems WmsMapExceptions WmsVersion where
  toQueryItems Wms111 exc = [("EXCEPTIONS", val)]
    where
      val = case exc of
              WmsMapExcXml     -> "application/vnd.ogc_sexml"
              WmsMapExcInImage -> "application/vnd.ogc_seinimage"
              WmsMapExcBlank   -> "application/vnd.ogc_seblank"
  toQueryItems _ exc = [("EXCEPTIONS", val)]
    where
      val = case exc of
              WmsMapExcXml     -> "XML"
              WmsMapExcInImage -> "INIMAGE"
              WmsMapExcBlank   -> "BLANK"
  {-# INLINE toQueryItems #-}

newtype UpdateSequence = UpdateSequence ByteString
  deriving (Eq, Ord, Show, IsString)

instance FromQuery (Maybe UpdateSequence) () where
  fromQuery () = optionalParameter
    "UPDATESEQUENCE" (UpdateSequence <$> takeByteString)
  {-# INLINE fromQuery #-}

instance ToQueryItems UpdateSequence a where
  toQueryItems _ (UpdateSequence s)  = [("UPDATESEQUENCE", s)]
  {-# INLINE toQueryItems #-}

parseRequest :: Request -> Either OgcRequestError OgcRequest
parseRequest req = do
  service <- fromQuery_ query
  case service of
    WCS -> Left NotImplementedError
    WFS -> Left NotImplementedError
    WMS -> parseWmsRequest req
    WPS -> Left NotImplementedError
  where
    query = queryStringCI req

-- | Renders a OgcRequest into a 'SimpleQuery' and an optional request body
--   (eg: for WFS POST requests)
renderRequest :: OgcRequest -> (SimpleQuery, Maybe ByteString)
renderRequest WmsCapabilitiesRequest {..} = (query, Nothing)
  where
    query = concat [
        toQueryItems version WMS
      , toQueryItems version GetCapabilities
      , toQueryItems version wmsCapVersion
      , toQueryItems version wmsCapFormat
      , toQueryItems version wmsCapUpdateSequence
      ]
    version = fromMaybe Wms130 wmsCapVersion
renderRequest WmsMapRequest {..} = (query, Nothing)
  where
    query = concat [
        toQueryItems wmsMapVersion WMS
      , toQueryItems wmsMapVersion GetMap
      , toQueryItems wmsMapVersion wmsMapVersion
      , toQueryItems wmsMapVersion wmsMapLayers
      , toQueryItems wmsMapVersion wmsMapCrs
      , toQueryItems wmsMapVersion wmsMapBbox
      , toQueryItems wmsMapVersion wmsMapSize
      , toQueryItems wmsMapVersion wmsMapFormat
      , toQueryItems wmsMapVersion wmsMapTransparent
      , toQueryItems wmsMapVersion wmsMapBackground
      , toQueryItems wmsMapVersion wmsMapExceptions
      , toQueryItems wmsMapVersion wmsMapTime
      , toQueryItems wmsMapVersion wmsMapElevation
      , toQueryItems wmsMapVersion wmsMapDimensions
      ]

renderRequest _ = error "renderRequest: not implemented"

--
-- * Internal types and functions
--

class FromQuery a c  where
  fromQuery :: c -> QueryCI -> Either OgcRequestError a

fromQuery_ :: FromQuery a () => QueryCI -> Either OgcRequestError a
fromQuery_ = fromQuery ()

class ToQueryItems a c  where
  toQueryItems :: c -> a -> [(ByteString, ByteString)]

instance {-# OVERLAPPABLE #-} ToQueryItems a c => ToQueryItems (Maybe a) c where
  toQueryItems c = maybe [] (toQueryItems c)
  {-# INLINE toQueryItems #-}

data WmsRequest
  = GetCapabilities
  | GetMap
  | GetFeatureInfo
  deriving (Show, Typeable)

instance FromQuery WmsRequest (Maybe WmsVersion) where
  fromQuery version = mandatoryParameter "REQUEST" parser
    where
      parser = case version of
        Just Wms100 -> wms100RequestParser
        Just _      -> wmsRequestParser
        Nothing     -> wmsRequestParser <|> wms100RequestParser
      wms100RequestParser = choice [
          stringCI "capabilities" *> pure GetCapabilities
        , stringCI "map"          *> pure GetMap
        , stringCI "feature_info" *> pure GetFeatureInfo
        ]
      wmsRequestParser = choice [
          stringCI "GetCapabilities" *> pure GetCapabilities
        , stringCI "GetMap"          *> pure GetMap
        , stringCI "GetFeatureInfo"  *> pure GetFeatureInfo
        ]
  {-# INLINE fromQuery #-}

instance ToQueryItems WmsRequest WmsVersion where
  toQueryItems Wms100 GetCapabilities = [("REQUEST", "capabilities")]
  toQueryItems Wms100 GetMap          = [("REQUEST", "map")]
  toQueryItems Wms100 GetFeatureInfo  = [("REQUEST", "feature_info")]
  toQueryItems _      req             = [("REQUEST", fromString (show req))]
  {-# INLINE toQueryItems #-}

type QueryCI = [(CI ByteString, Maybe ByteString)]

data Service = WCS | WFS | WMS | WPS
  deriving (Eq, Show, Enum, Bounded)

instance FromQuery Service () where
  fromQuery () = mandatoryParameter "SERVICE" $
    choice [
      stringCI "WCS" *> pure WCS
    , stringCI "WFS" *> pure WFS
    , stringCI "WMS" *> pure WMS
    , stringCI "WPS" *> pure WPS
    ]
  {-# INLINE fromQuery #-}

instance ToQueryItems Service c where
  toQueryItems _ s = [("SERVICE", fromString (show s))]
  {-# INLINE toQueryItems #-}


parseWmsRequest :: Request -> Either OgcRequestError OgcRequest
parseWmsRequest req = do
  version <- reqWmsVersion
  request <- fromQuery version query
  case request of
    GetCapabilities -> parseWmsCapabilitiesRequest version
    GetMap          -> parseWmsMapRequest version
    GetFeatureInfo  -> Left NotImplementedError
  where
    query = queryStringCI req

    parseWmsCapabilitiesRequest wmsCapVersion = do
      wmsCapFormat <- fromQuery_ query
      wmsCapUpdateSequence <- fromQuery_ query
      return WmsCapabilitiesRequest {..}

    parseWmsMapRequest Nothing = Left (MissingParameterError "VERSION")
    parseWmsMapRequest (Just wmsMapVersion) = do
      wmsMapLayers <- fromQuery_ query
      wmsMapCrs <- fromQuery wmsMapVersion query
      wmsMapBbox <- fromQuery_ query
      wmsMapSize <- fromQuery_ query
      wmsMapFormat <- fromQuery_ query
      wmsMapTransparent <- fromQuery_ query
      wmsMapBackground <- fromQuery_ query
      wmsMapExceptions <- fromQuery wmsMapVersion query
      wmsMapTime <- fromQuery_ query
      wmsMapElevation <- fromQuery_ query
      wmsMapDimensions <- fromQuery_ query
      return WmsMapRequest {..}

    reqWmsVersion =
      case fromQuery Wms100 query of
        Right Nothing ->
          case fromQuery Wms111 query of
            Left _ -> fromQuery Wms130 query
            r -> r
        r -> r

instance FromQuery SR.Crs WmsVersion where
  fromQuery Wms130 = reqCrs "CRS"
  fromQuery _      = reqCrs "SRS"
  {-# INLINE fromQuery #-}

instance ToQueryItems SR.Crs WmsVersion where
  toQueryItems Wms130 (Coded code val) =
    [("CRS", runBuilder (fromString code) <> ":" <> show' val)]
  toQueryItems _ (Coded code val) =
    [("SRS", runBuilder (fromString code) <> ":" <> show' val)]
  toQueryItems Wms130 (Named name) = [("CRS", fromString name)]
  toQueryItems _      (Named name) = [("SRS", fromString name)]
  toQueryItems _      _            = error "toQueryItems: Crs"
  {-# INLINE toQueryItems #-}

reqCrs :: CI ByteString -> QueryCI -> Either OgcRequestError SR.Crs
reqCrs key = mandatoryParameter key (epsg <|> named)
  where
    epsg, named :: Parser SR.Crs
    named  = namedCrs <$> (BS.unpack <$> takeByteString)
    epsg = maybe (fail "invalid epsg") return
       =<< (SR.epsgCrs <$> (stringCI "EPSG:" *> decimal <* endOfInput))



instance FromQuery (Maybe MIME.Type) () where
  fromQuery () query =
    case L.lookup "FORMAT" query of
      Just (Just fmt)
        | Just mime <- parseMIME fmt -> Right (Just mime)
        | otherwise -> Left  (InvalidParameterError "FORMAT" fmt)
      Just Nothing  -> Left  (EmptyParameterError "FORMAT")
      Nothing       -> Right Nothing
    where
      parseMIME = MIME.parseMIMEType . lenientDecodeUtf8
  {-# INLINE fromQuery #-}

instance FromQuery MIME.Type () where
  fromQuery ()
    = either Left (maybe (Left (MissingParameterError "FORMAT")) Right)
    . fromQuery ()
  {-# INLINE fromQuery #-}

instance ToQueryItems MIME.Type c where
  toQueryItems _ a = [("FORMAT", T.encodeUtf8 (MIME.showType a))]
  {-# INLINE toQueryItems #-}

--
-- * Utils
--

posInt :: Parser Int
posInt = do
  n <- decimal
  if n==0 then fail "Negative number" else return n

lenientDecodeUtf8 :: ByteString -> Text
lenientDecodeUtf8 = T.decodeUtf8With T.lenientDecode

mandatoryParameter
  :: CI ByteString
  -> Parser a
  -> QueryCI
  -> Either OgcRequestError a
mandatoryParameter key parser
  = either Left (maybe (Left (MissingParameterError (CI.original key))) Right)
  . optionalParameter key parser

optionalParameter
  :: CI ByteString
  -> Parser a
  -> QueryCI
  -> Either OgcRequestError (Maybe a)
optionalParameter key parser query =
  case L.lookup key query of
    Just (Just "") -> Left (EmptyParameterError (CI.original key))
    Just (Just s) ->
      either (const (Left (InvalidParameterError (CI.original key) s)))
             (Right . Just)
             (parseOnly (parser <* endOfInput) s)
    Just Nothing -> Left (EmptyParameterError (CI.original key))
    Nothing      -> Right Nothing


-- | Parses a query string into case-insensitive elements
queryStringCI :: Request -> QueryCI
queryStringCI = map (first CI.mk) . queryString

showScientific :: IsString a => Scientific -> a
showScientific = show'

runBuilder :: Builder -> ByteString
runBuilder = LBS.toStrict . toLazyByteString

show' :: (Show a, IsString b) => a -> b
show' = fromString . show
