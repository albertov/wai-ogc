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
{-# LANGUAGE GADTs #-}

module Network.Wai.Ogc.Wms (
    Request     (..)
  , Version     (..)
  , Exceptions  (..)
  , BgColor     (..)
  , Transparent (..)
  , Layer       (..)
  , Pixel       (..)

  -- * 'Request' constructors
  , wmsCapabilitiesRequest
  , wmsMapRequest

  -- * Smart constructors
  , mkLayer

  -- * Re-exports
  , module Common
) where

import qualified Network.Wai.Ogc.Common as Common
import           Network.Wai.Ogc.Common hiding (Request)

import           Control.Applicative ((<|>))
import           Control.Monad (liftM)
import qualified Codec.MIME.Type as MIME
import           Data.Attoparsec.ByteString.Char8 as AP
import           Data.ByteString (ByteString)
import           Data.ByteString.Builder (word8HexFixed)
import qualified Data.ByteString.Char8 as BS
import           Data.ByteString.Lex.Integral (readHexadecimal)
import           Data.CaseInsensitive (CI)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.String (fromString)
import           Data.Text (Text)
import           Data.Typeable (Typeable)
import           Data.Word (Word8)
import           Network.HTTP.Types.URI (simpleQueryToQuery)

--
-- * Public types and functions
--

data RequestType
  = GetCapabilities
  | GetMap
  | GetFeatureInfo
  deriving (Show, Typeable)

data Request
  = CapabilitiesRequest {
      wmsCapFormat         :: Maybe MIME.Type
    , wmsCapVersion        :: Maybe Version
    , wmsCapUpdateSequence :: Maybe UpdateSequence
    }
  | MapRequest {
      wmsMapVersion      :: Version
    , wmsMapLayers       :: [Layer]
    , wmsMapCrs          :: Crs
    , wmsMapBbox         :: Bbox
    , wmsMapSize         :: Size
    , wmsMapFormat       :: MIME.Type
    , wmsMapTransparent  :: Maybe Transparent
    , wmsMapBackground   :: Maybe BgColor
    , wmsMapExceptions   :: Maybe Exceptions
    , wmsMapTime         :: Maybe Time
    , wmsMapElevation    :: Maybe Scientific
    , wmsMapDimensions   :: [Dimension]
    }
  | FeatureInfoRequest {
      wmsFeatureInfoVersion      :: Version
    , wmsFeatureInfoLayers       :: [Layer]
    , wmsFeatureInfoCrs          :: Crs
    , wmsFeatureInfoBbox         :: Bbox
    , wmsFeatureInfoSize         :: Size
    , wmsFeatureInfoFormat       :: MIME.Type
    , wmsQueryLayers  :: [Name]
    , wmsInfoFormat   :: MIME.Type
    , wmsPixel        :: Pixel
    , wmsFeatureCount :: Maybe Int
    , wmsFeatureInfoTransparent  :: Maybe Transparent
    , wmsFeatureInfoBackground   :: Maybe BgColor
    , wmsFeatureInfoExceptions   :: Maybe Exceptions
    , wmsFeatureInfoTime         :: Maybe Time
    , wmsFeatureInfoElevation    :: Maybe Scientific
    , wmsFeatureInfoDimensions   :: [Dimension]
    }
  deriving (Eq, Show)

{-
data Request (t :: RequestType -> *) where
  = CapabilitiesRequest {
      wmsCapFormat         :: Maybe MIME.Type
    , wmsCapVersion        :: Maybe Version
    , wmsCapUpdateSequence :: Maybe UpdateSequence
    } -> Request GetCapabilities
  | MapRequest {
      wmsMapVersion      :: Version
    , wmsMapLayers       :: [Layer]
    , wmsMapCrs          :: Crs
    , wmsMapBbox         :: Bbox
    , wmsMapSize         :: Size
    , wmsMapFormat       :: MIME.Type
    , wmsMapTransparent  :: Maybe Transparent
    , wmsMapBackground   :: Maybe BgColor
    , wmsMapExceptions   :: Maybe Exceptions
    , wmsMapTime         :: Maybe Time
    , wmsMapElevation    :: Maybe Scientific
    , wmsMapDimensions   :: [Dimension]
    } -> Request GetMap
  | FeatureInfoRequest {
      wmsFeatureInfoVersion      :: Version
    , wmsFeatureInfoLayers       :: [Layer]
    , wmsFeatureInfoCrs          :: Crs
    , wmsFeatureInfoBbox         :: Bbox
    , wmsFeatureInfoSize         :: Size
    , wmsFeatureInfoFormat       :: MIME.Type
    , wmsQueryLayers  :: [Name]
    , wmsInfoFormat   :: MIME.Type
    , wmsPixel        :: Pixel
    , wmsFeatureCount :: Maybe Int
    , wmsFeatureInfoTransparent  :: Maybe Transparent
    , wmsFeatureInfoBackground   :: Maybe BgColor
    , wmsFeatureInfoExceptions   :: Maybe Exceptions
    , wmsFeatureInfoTime         :: Maybe Time
    , wmsFeatureInfoElevation    :: Maybe Scientific
    , wmsFeatureInfoDimensions   :: [Dimension]
    } -> Request GetFeatureInfo
  deriving (Eq, Show)
  -}


wmsCapabilitiesRequest :: Request
wmsCapabilitiesRequest = CapabilitiesRequest Nothing Nothing Nothing

wmsMapRequest
  :: Version
  -> MIME.Type
  -> [Layer]
  -> Crs
  -> Size
  -> Bbox
  -> Request
wmsMapRequest version format layers crs size bbox =
  MapRequest {
    wmsMapVersion      = version
  , wmsMapLayers       = layers
  , wmsMapCrs          = crs
  , wmsMapBbox         = bbox
  , wmsMapSize         = size
  , wmsMapFormat       = format
  , wmsMapTransparent  = Nothing
  , wmsMapBackground   = Nothing
  , wmsMapExceptions   = Nothing
  , wmsMapTime         = Nothing
  , wmsMapElevation    = Nothing
  , wmsMapDimensions   = []
  }


--
-- * Layer
--

data Layer =
  Layer {
    layerName :: Name
  , layerStyle :: Name
  }
  deriving (Eq, Show)

mkLayer :: Text -> Text -> Maybe Layer
mkLayer name style = Layer <$> mkName name <*> mkName style

instance FromQuery [Layer] () where
  fromQuery () query = do
    layers <- namesFromQuery "LAYERS" query
    styles <- namesFromQuery "STYLES" query
    if length layers == length styles
      then Right (zipWith Layer layers styles)
      else Left LayersStylesLengthMismatchError
  {-# INLINE fromQuery #-}

instance ToQueryItems [Layer] c where
  toQueryItems _ ls =
    [ ("LAYERS", renderNames (map layerName ls))
    , ("STYLES", renderNames (map layerStyle ls))
    ]
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
-- * Pixel
--

data Pixel =
  Pixel {
    pixelRow :: !Int
  , pixelCol :: !Int
  } deriving (Eq, Show)

instance FromQuery Pixel () where
  fromQuery () query =
    Pixel <$> mandatoryParameter "I" (signed decimal) query
          <*> mandatoryParameter "J" (signed decimal) query
  {-# INLINE fromQuery #-}

instance ToQueryItems Pixel c where
  toQueryItems _ (Pixel i j) =
    [("I", fromString (show i)) , ("J", fromString (show j))]
  {-# INLINE toQueryItems #-}


--
-- * Version
--

data Version
  = Wms100
  | Wms111
  | Wms130
  deriving (Show, Eq, Ord, Enum, Bounded)

instance FromQuery (Maybe Version) Version where
  fromQuery Wms100 = optionalParameter "WMSTVER"  parser
    where parser = ("1.0.0" <|> "1.0" <|> "1") *> pure Wms100
  fromQuery Wms111 = optionalParameter "VERSION"  parser
    where parser = "1.1.1" *> pure Wms111
  fromQuery Wms130 = optionalParameter "VERSION"  parser
    where parser = ("1.3.0" <|> "1.3") *> pure Wms130
  {-# INLINE fromQuery #-}

instance ToQueryItems Version Version where
  toQueryItems Wms100 _ = [("WMSTVER", "1.0.0")]
  toQueryItems Wms111 _ = [("VERSION", "1.1.1")]
  toQueryItems Wms130 _ = [("VERSION", "1.3.0")]
  {-# INLINE toQueryItems #-}


--
-- * Exceptions
--

data Exceptions = ExcXml | ExcInImage | ExcBlank
  deriving (Show, Eq, Ord, Enum, Bounded)

instance FromQuery (Maybe Exceptions) Version where
  fromQuery version = optionalParameter "EXCEPTIONS" parser
    where
      parser = case version of
                 Wms100 -> simpleParser
                 Wms130 -> simpleParser
                 Wms111 -> wms111Parser
      simpleParser = stringCI "XML"     *> pure ExcXml
                 <|> stringCI "INIMAGE" *> pure ExcInImage
                 <|> stringCI "BLANK"   *> pure ExcBlank
      wms111Parser = stringCI "application/vnd.ogc_se" *> simpleParser
  {-# INLINE fromQuery #-}

instance ToQueryItems Exceptions Version where
  toQueryItems Wms111 exc = [("EXCEPTIONS", val)]
    where
      val = case exc of
              ExcXml     -> "application/vnd.ogc_sexml"
              ExcInImage -> "application/vnd.ogc_seinimage"
              ExcBlank   -> "application/vnd.ogc_seblank"
  toQueryItems _ exc = [("EXCEPTIONS", val)]
    where
      val = case exc of
              ExcXml     -> "XML"
              ExcInImage -> "INIMAGE"
              ExcBlank   -> "BLANK"
  {-# INLINE toQueryItems #-}


instance FromQuery RequestType (Maybe Version) where
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

instance ToQueryItems RequestType Version where
  toQueryItems Wms100 GetCapabilities = [("REQUEST", "capabilities")]
  toQueryItems Wms100 GetMap          = [("REQUEST", "map")]
  toQueryItems Wms100 GetFeatureInfo  = [("REQUEST", "feature_info")]
  toQueryItems _      req             = [("REQUEST", fromString (show req))]
  {-# INLINE toQueryItems #-}



instance Common.Request Request where
  parseRequest (queryCI -> query) _ = do
    version <- reqVersion
    request <- fromQuery version query
    case request of
      GetCapabilities -> parseCapabilitiesRequest version
      GetMap          -> parseMapRequest version
      GetFeatureInfo  -> parseFeatureInfoRequest version
    where
      parseCapabilitiesRequest wmsCapVersion = do
        wmsCapFormat <- optionalParameter "FORMAT" mimeParser query
        wmsCapUpdateSequence <- fromQuery_ query
        return CapabilitiesRequest {..}

      parseMapRequest Nothing = Left (MissingParameterError "VERSION")
      parseMapRequest (Just wmsMapVersion) = do
        wmsMapLayers <- fromQuery_ query
        wmsMapCrs <- fromQuery wmsMapVersion query
        wmsMapBbox <- fromQuery_ query
        wmsMapSize <- fromQuery_ query
        wmsMapFormat <- mandatoryParameter "FORMAT" mimeParser query
        wmsMapTransparent <- fromQuery_ query
        wmsMapBackground <- fromQuery_ query
        wmsMapExceptions <- fromQuery wmsMapVersion query
        wmsMapTime <- fromQuery_ query
        wmsMapElevation <- optionalParameter "ELEVATION" scientific query
        wmsMapDimensions <- fromQuery_ query
        return MapRequest {..}

      parseFeatureInfoRequest Nothing = Left (MissingParameterError "VERSION")
      parseFeatureInfoRequest (Just wmsFeatureInfoVersion) = do
        wmsFeatureInfoLayers <- fromQuery_ query
        wmsFeatureInfoCrs <- fromQuery wmsFeatureInfoVersion query
        wmsFeatureInfoBbox <- fromQuery_ query
        wmsFeatureInfoSize <- fromQuery_ query
        wmsFeatureInfoFormat <- mandatoryParameter "FORMAT" mimeParser query
        wmsFeatureInfoTransparent <- fromQuery_ query
        wmsFeatureInfoBackground <- fromQuery_ query
        wmsFeatureInfoExceptions <- fromQuery wmsFeatureInfoVersion query
        wmsFeatureInfoTime <- fromQuery_ query
        wmsFeatureInfoElevation <- optionalParameter "ELEVATION" scientific query
        wmsFeatureInfoDimensions <- fromQuery_ query
        wmsQueryLayers <- namesFromQuery "QUERY_LAYERS" query
        wmsFeatureCount <- optionalParameter "FEATURE_COUNT" positiveInt query
        wmsPixel <- fromQuery_ query
        wmsInfoFormat <- mandatoryParameter "INFO_FORMAT" mimeParser query
        return FeatureInfoRequest {..}

      reqVersion =
        case fromQuery Wms100 query of
          Right Nothing ->
            case fromQuery Wms111 query of
              Left _ -> fromQuery Wms130 query
              r -> r
          r -> r

  renderRequest CapabilitiesRequest {..} = (query, Nothing)
    where
      query = simpleQueryToQuery $ concat [
          toQueryItems version WMS
        , toQueryItems version GetCapabilities
        , toQueryItems version wmsCapVersion
        , maybe [] (\fmt -> [("FORMAT", renderMime fmt)]) wmsCapFormat
        , toQueryItems version wmsCapUpdateSequence
        ]
      version = fromMaybe Wms130 wmsCapVersion
  renderRequest MapRequest {..} = (query, Nothing)
    where
      query = simpleQueryToQuery $ concat [
          toQueryItems wmsMapVersion WMS
        , toQueryItems wmsMapVersion GetMap
        , toQueryItems wmsMapVersion wmsMapVersion
        , toQueryItems wmsMapVersion wmsMapLayers
        , toQueryItems wmsMapVersion wmsMapCrs
        , toQueryItems wmsMapVersion wmsMapBbox
        , toQueryItems wmsMapVersion wmsMapSize
        , [("FORMAT", renderMime wmsMapFormat)]
        , toQueryItems wmsMapVersion wmsMapTransparent
        , toQueryItems wmsMapVersion wmsMapBackground
        , toQueryItems wmsMapVersion wmsMapExceptions
        , toQueryItems wmsMapVersion wmsMapTime
        , renderOptionalParameter "ELEVATION" (fmap show' wmsMapElevation)
        , [("FORMAT", renderMime wmsMapFormat)]
        , toQueryItems wmsMapVersion wmsMapDimensions
        ]
  renderRequest FeatureInfoRequest {..} = (query, Nothing)
    where
      query = simpleQueryToQuery $ concat [
          toQueryItems wmsFeatureInfoVersion WMS
        , toQueryItems wmsFeatureInfoVersion GetFeatureInfo
        , toQueryItems wmsFeatureInfoVersion wmsFeatureInfoVersion
        , toQueryItems wmsFeatureInfoVersion wmsFeatureInfoLayers
        , toQueryItems wmsFeatureInfoVersion wmsFeatureInfoCrs
        , toQueryItems wmsFeatureInfoVersion wmsFeatureInfoBbox
        , toQueryItems wmsFeatureInfoVersion wmsFeatureInfoSize
        , [("FORMAT", renderMime wmsFeatureInfoFormat)]
        , toQueryItems wmsFeatureInfoVersion wmsFeatureInfoTransparent
        , toQueryItems wmsFeatureInfoVersion wmsFeatureInfoBackground
        , toQueryItems wmsFeatureInfoVersion wmsFeatureInfoExceptions
        , toQueryItems wmsFeatureInfoVersion wmsFeatureInfoTime
        , renderOptionalParameter "ELEVATION" (fmap show' wmsFeatureInfoElevation)
        , toQueryItems wmsFeatureInfoVersion wmsFeatureInfoDimensions
        , [ ("QUERY_LAYERS", renderNames wmsQueryLayers)
          , ("INFO_FORMAT", renderMime wmsInfoFormat)
          ]
        , renderOptionalParameter "FEATURE_COUNT" (fmap show' wmsFeatureCount)
        , toQueryItems wmsFeatureInfoVersion wmsPixel
        ]

instance FromQuery Crs Version where
  fromQuery Wms130 = reqCrs "CRS"
  fromQuery _      = reqCrs "SRS"
  {-# INLINE fromQuery #-}

instance ToQueryItems Crs Version where
  toQueryItems Wms130 (Coded code val) =
    [("CRS", runBuilder (fromString code <> ":" <> show' val))]
  toQueryItems _ (Coded code val) =
    [("SRS", runBuilder (fromString code <> ":" <> show' val))]
  toQueryItems Wms130 (Named name) = [("CRS", fromString name)]
  toQueryItems _      (Named name) = [("SRS", fromString name)]
  toQueryItems _      _            = [] -- let the remote server decide
  {-# INLINE toQueryItems #-}

reqCrs :: CI ByteString -> QueryCI -> Either ParseError Crs
reqCrs key =
  mandatoryParameter key $ coded "EPSG"
                       <|> coded "CRS"
                       <|> coded "SR-ORG"
                       <|> named
  where
    named = namedCrs <$> (BS.unpack <$> takeByteString)
    coded code = maybe (fail "invalid coded crs") return =<< mCoded code
    mCoded code =
      codedCrs <$> (BS.unpack <$> stringCI code <* char ':')
               <*> decimal <* endOfInput
