{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}

module Network.Wai.Ogc (
    OgcRequest       (..)
  , OgcRequestError  (..)
  , WmsVersion       (..)
  , WmsMapExceptions (..)
  , Layer
  , Service          (..)
  , Bbox
  , Size
  , BgColor          (..)
  , Transparent      (..)
  , Time             (..)
  , TimeStamp        (..)
  , Elevation        (..)
  , Dimension        (..)
  , UpdateSequence

  -- * 'OgcRequest' constructors
  , wmsCapabilitiesRequest
  , wmsMapRequest

  -- * 'OgcRequest' parse and render
  , parseRequest
  , renderRequest

  -- * Smart constructors
  , mkLayer
  , mkSize
  , mkBbox

  -- * Accessors

  -- ** 'Layer'
  , layerName
  , layerStyle

  -- ** 'Bbox'
  , minx
  , miny
  , maxx
  , maxy

  -- ** 'Size
  , width
  , height

  -- ** Utils
  , timeParser
  , parseTime
  , renderTime

  -- * Re-exports
  , MIME.Type (..)
  , MIME.MIMEType (..)
  , MIME.MIMEParam (..)
  , Scientific
  , module SR
  , module Duration
) where

import           Network.Wai.Ogc.Internal.Duration as Duration

import           Control.Applicative ((<|>), optional)
import           Control.Arrow (first)
import           Control.Monad (liftM)
import qualified Codec.MIME.Parse as MIME
import qualified Codec.MIME.Type as MIME
import           Data.Attoparsec.ByteString.Char8 as AP
import           Data.ByteString (ByteString)
import           Data.ByteString.Builder ( Builder
                                         , word8HexFixed
                                         , toLazyByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.ByteString.Lex.Integral (readDecimal, readHexadecimal)
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
import           Data.Time (UTCTime(..), defaultTimeLocale)
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.Format (formatTime)
import           Data.Typeable (Typeable)
import           Data.Word (Word8)
import           SpatialReference as SR
import           Network.Wai (Request, queryString)
import           Network.HTTP.Types.URI (SimpleQuery)

--
-- * Public types and functions
--

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
    , wmsMapTime             :: Maybe Time
    , wmsMapElevation        :: Maybe Elevation
    , wmsMapDimensions       :: [Dimension]
    }
  | WpsRequest
  deriving (Eq, Show)

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

data OgcRequestError
  = MissingParameterError           ByteString
  | EmptyParameterError             ByteString
  | InvalidParameterError           ByteString ByteString
  | LayersStylesLengthMismatchError
  | NotImplementedError
  deriving (Show, Typeable)

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
-- * Layer
--

data Layer = Layer Text Text
  deriving (Eq, Show)

mkLayer :: Text -> Text -> Maybe Layer
mkLayer name style
  | Nothing <- T.findIndex (==',') name
  , Nothing <- T.findIndex (==',') style = Just (Layer name style)
mkLayer _ _ = Nothing

layerName :: Layer -> Text
layerName (Layer n _) = n

layerStyle :: Layer -> Text
layerStyle (Layer _ s) = s

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

--
-- * Bbox
--

data Bbox = Bbox !Scientific !Scientific !Scientific !Scientific
  deriving (Eq, Show)

mkBbox :: Scientific -> Scientific -> Scientific -> Scientific -> Maybe Bbox
mkBbox x0 y0 x1 y1
  | x1>x0 && y1>y0 = Just (Bbox x0 y0 x1 y1)
  | otherwise      = Nothing

minx, miny, maxx, maxy :: Bbox -> Scientific
minx (Bbox a _ _ _) = a
miny (Bbox _ a _ _) = a
maxx (Bbox _ _ a _) = a
maxy (Bbox _ _ _ a) = a

instance FromQuery Bbox () where
  fromQuery () = mandatoryParameter "BBOX" $ do
    x0 <- scientific <* char ','
    y0 <- scientific <* char ','
    x1 <- scientific <* char ','
    y1 <- scientific
    maybe (fail "Invalid bbox") return (mkBbox x0 y0 x1 y1)
  {-# INLINE fromQuery #-}

instance ToQueryItems Bbox c where
  toQueryItems _ (Bbox a b c d) = [ ("BBOX", runBuilder bld)]
    where bld = mconcat (L.intersperse "," (map show' [a, b, c, d]))
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
  fromQuery () = optionalParameter "BGCOLOR" $
    BgColor <$> (stringCI "0x" *> parseHex) <*> parseHex <*> parseHex
    where
      parseHex = maybe (fail "not an hex") (return . fst)
             =<< liftM readHexadecimal (AP.take 2)
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
    optionalParameter "TRANSPARENT"
      (stringCI "TRUE"  *> pure Transparent <|>
       stringCI "FALSE" *> pure Opaque)
  {-# INLINE fromQuery #-}

instance ToQueryItems Transparent c where
  toQueryItems _ Transparent = [("TRANSPARENT", "TRUE")]
  toQueryItems _ Opaque      = [("TRANSPARENT", "FALSE")]
  {-# INLINE toQueryItems #-}

--
-- * Size
--

data Size = Size !Int !Int
  deriving (Eq, Show)

mkSize :: Int -> Int -> Maybe Size
mkSize w h
  | w>0 && h>0 = Just (Size w h)
  | otherwise  = Nothing

width, height :: Size -> Int
width  (Size w _) = w
height (Size _ h) = h


instance FromQuery Size () where
  fromQuery () query =
    Size <$> mandatoryParameter "WIDTH" posInt query
         <*> mandatoryParameter "HEIGHT" posInt query
  {-# INLINE fromQuery #-}

instance ToQueryItems Size c where
  toQueryItems _ (Size w h) =
    [("WIDTH", fromString (show w)) , ("HEIGHT", fromString (show h))]
  {-# INLINE toQueryItems #-}



--
-- * Elevation
--

newtype Elevation = Elevation Scientific
  deriving (Eq, Ord, Show, Num)

instance FromQuery (Maybe Elevation) () where
  fromQuery () = optionalParameter "ELEVATION" (Elevation <$> scientific)
  {-# INLINE fromQuery #-}

instance ToQueryItems Elevation c where
  toQueryItems _ (Elevation e) = [("ELEVATION", show' e)]
  {-# INLINE toQueryItems #-}

--
-- * TimeInterval
--

data TimeStamp = TimeStamp UTCTime | Current
  deriving (Eq, Show)

data Time
  = Time     TimeStamp
  | Interval TimeStamp TimeStamp (Maybe Duration)
  deriving (Eq, Show)

instance FromQuery (Maybe Time) () where
  fromQuery () = optionalParameter "TIME" timeParser
  {-# INLINE fromQuery #-}

parseTime :: ByteString -> Either String Time
parseTime = parseOnly (timeParser <* endOfInput)

timeParser :: Parser Time
timeParser = parseInterval <|> (Time <$> parseTimeStamp)
  where
    parseInterval =
      Interval
        <$> parseTimeStamp <* char '/'
        <*> parseTimeStamp
        <*> (endOfInput *> pure Nothing <|> Just <$> (char '/' *> duration))

    parseTimeStamp = (stringCI "current" *> pure Current) <|> parseISO

    parseISO = do
      day <- parseDay1 <|> parseDay2
      dt <- option 0 (oChar 'T' *> parseDiffTime <* oChar 'Z')
      return (TimeStamp (UTCTime day dt))

    parseDay1 = fromGregorian <$> (fromIntegral <$> parseIntN 4 <* char '-')
                              <*> parseIntN 2
                              <*> option 1 (char '-' *> parseIntN 2)

    parseDay2 = fromGregorian <$> (fromIntegral <$> parseIntN 4 <* oChar '-')
                              <*> parseIntN 2 <* oChar '-'
                              <*> parseIntN 2

    parseDiffTime = do
      h <- fromIntegral <$> parseIntN 2
      m <- fromIntegral <$> option 0 (oChar ':' *> parseIntN 2)
      s <- maybe 0 realToFrac <$> optional (oChar ':' *> scientific)
      return (s + m*60 + h*3600)

    parseIntN :: Int -> Parser Int
    parseIntN n =
      maybe (fail "not an int") (return . fst) =<< liftM readDecimal (AP.take n)

    oChar = optional . char
{-# INLINE timeParser #-}

renderTime :: Time -> ByteString
renderTime = \case
  Time (format -> t) -> t
  Interval (format -> s) (format -> e) Nothing ->
    runBuilder (s <> "/" <> e)
  Interval (format -> s) (format -> e) (Just (formatDurationB -> d)) ->
    runBuilder (s <> "/" <> e <> "/" <> d)
  where
    format Current = "current"
    format (TimeStamp t) =
      fromString (formatTime defaultTimeLocale "%FT%T%QZ" t)
{-# INLINE renderTime #-}

instance ToQueryItems Time c where
  toQueryItems _ t = [("TIME", renderTime t)]
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


--
-- * WmsVersion
--

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
  toQueryItems Wms100 _ = [("WMSTVER", "1.0.0")]
  toQueryItems Wms111 _ = [("VERSION", "1.1.1")]
  toQueryItems Wms130 _ = [("VERSION", "1.3.0")]
  {-# INLINE toQueryItems #-}


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
      simpleParser = stringCI "XML"     *> pure WmsMapExcXml
                 <|> stringCI "INIMAGE" *> pure WmsMapExcInImage
                 <|> stringCI "BLANK"   *> pure WmsMapExcBlank
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

--
-- * UpdateSequence
--

newtype UpdateSequence = UpdateSequence ByteString
  deriving (Eq, Ord, Show, IsString)

instance FromQuery (Maybe UpdateSequence) () where
  fromQuery () = optionalParameter
    "UPDATESEQUENCE" (UpdateSequence <$> takeByteString)
  {-# INLINE fromQuery #-}

instance ToQueryItems UpdateSequence a where
  toQueryItems _ (UpdateSequence s)  = [("UPDATESEQUENCE", s)]
  {-# INLINE toQueryItems #-}


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
      wms100RequestParser =
            stringCI "capabilities" *> pure GetCapabilities
        <|> stringCI "map"          *> pure GetMap
        <|> stringCI "feature_info" *> pure GetFeatureInfo
      wmsRequestParser =
            stringCI "GetCapabilities" *> pure GetCapabilities
        <|> stringCI "GetMap"          *> pure GetMap
        <|> stringCI "GetFeatureInfo"  *> pure GetFeatureInfo
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
        stringCI "WCS" *> pure WCS
    <|> stringCI "WFS" *> pure WFS
    <|> stringCI "WMS" *> pure WMS
    <|> stringCI "WPS" *> pure WPS
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
    [("CRS", runBuilder (fromString code <> ":" <> show' val))]
  toQueryItems _ (Coded code val) =
    [("SRS", runBuilder (fromString code <> ":" <> show' val))]
  toQueryItems Wms130 (Named name) = [("CRS", fromString name)]
  toQueryItems _      (Named name) = [("SRS", fromString name)]
  toQueryItems _      _            = [] -- let the remote server decide
  {-# INLINE toQueryItems #-}

reqCrs :: CI ByteString -> QueryCI -> Either OgcRequestError SR.Crs
reqCrs key =
  mandatoryParameter key $ coded "EPSG"
                       <|> coded "CRS"
                       <|> coded "SR-ORG"
                       <|> named
  where
    named = namedCrs <$> (BS.unpack <$> takeByteString)
    coded code = maybe (fail "invalid coded crs") return =<< mCoded code
    mCoded code =
      SR.codedCrs <$> (BS.unpack <$> stringCI code <* char ':')
                  <*> decimal <* endOfInput



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
    Just (Just "") -> Left (EmptyParameterError oKey)
    Just (Just s) ->
      either (const (Left (InvalidParameterError oKey s)))
             (Right . Just)
             (parseOnly (parser <* endOfInput) s)
    Just Nothing -> Left (EmptyParameterError oKey)
    Nothing      -> Right Nothing
  where
    oKey = CI.original key

-- | Parses a query string into case-insensitive elements
queryStringCI :: Request -> QueryCI
queryStringCI = map (first CI.mk) . queryString

runBuilder :: Builder -> ByteString
runBuilder = LBS.toStrict . toLazyByteString

show' :: (Show a, IsString b) => a -> b
show' = fromString . show
