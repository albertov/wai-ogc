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
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}


module Network.Wai.Ogc.Wms (
    Request     (..)
  , SomeRequest (..)
  , Version     (..)
  , Exceptions  (..)
  , BgColor     (..)
  , Transparent (..)
  , Layer       (..)
  , Pixel       (..)
  , Capabilities
  , Map
  , FeatureInfo

  -- * 'Request' constructors
  , wmsCapabilitiesRequest
  , wmsMapRequest
  , wmsFeatureInfoRequest

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
import           Data.ByteString.Builder (word8HexFixed)
import           Data.ByteString.Lex.Integral (readHexadecimal)
import qualified Data.List as L
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
  = Capabilities
  | Map
  | FeatureInfo
  deriving (Show, Typeable)

type Capabilities = 'Capabilities
type Map          = 'Map
type FeatureInfo  = 'FeatureInfo

data SomeRequest where
  SomeRequest
    :: forall a. (Common.Request (Request a), Show (Request a), Eq (Request a))
     => Request a -> SomeRequest

instance Show SomeRequest where
  show (SomeRequest r) = show r

instance Eq SomeRequest where
  SomeRequest a@GetMap{}          == SomeRequest b@GetMap{}          = a==b
  SomeRequest a@GetCapabilities{} == SomeRequest b@GetCapabilities{} = a==b
  SomeRequest a@GetFeatureInfo{}  == SomeRequest b@GetFeatureInfo{}  = a==b
  _                               == _                               = False

data Request (t :: RequestType) where
  GetCapabilities :: {
      wmsCapFormat         :: Maybe MIME.Type
    , wmsCapVersion        :: Maybe Version
    , wmsCapUpdateSequence :: Maybe UpdateSequence
    } -> Request Capabilities
  GetMap :: {
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
    } -> Request Map
  GetFeatureInfo :: {
      wmsInfoMapRequest :: Request Map
    , wmsQueryLayers    :: [Name]
    , wmsInfoFormat     :: MIME.Type
    , wmsPixel          :: Pixel
    , wmsFeatureCount   :: Maybe Int
    } -> Request FeatureInfo

deriving instance Show (Request t)
deriving instance Eq (Request t)


wmsCapabilitiesRequest :: Request Capabilities
wmsCapabilitiesRequest = GetCapabilities Nothing Nothing Nothing

wmsMapRequest
  :: Version
  -> MIME.Type
  -> [Layer]
  -> Crs
  -> Size
  -> Bbox
  -> Request Map
wmsMapRequest version format layers crs size bbox =
  GetMap {
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

wmsFeatureInfoRequest
  :: Request Map
  -> MIME.Type
  -> [Name]
  -> Pixel
  -> Request FeatureInfo
wmsFeatureInfoRequest mapRequest format infoLayers pixel =
  GetFeatureInfo {
      wmsInfoMapRequest = mapRequest
    , wmsQueryLayers    = infoLayers
    , wmsInfoFormat     = format
    , wmsPixel          = pixel
    , wmsFeatureCount   = Nothing
    }

instance Common.Request SomeRequest where
  parseRequest query@(queryCI -> query') body = do
    version <- reqVersion query'
    request <- fromQuery version query'
    case request of
      Capabilities -> SomeRequest <$>
        (parseRequest query body :: Either ParseError (Request Capabilities))
      Map          -> SomeRequest <$>
        (parseRequest query body :: Either ParseError (Request Map))
      FeatureInfo  -> SomeRequest <$>
        (parseRequest query body :: Either ParseError (Request FeatureInfo))

  renderRequest (SomeRequest r) = renderRequest r

instance Common.Request (Request Capabilities) where

  parseRequest (queryCI -> query) _ = do
    wmsCapVersion <- reqVersion query
    wmsCapFormat <- optionalParameter "FORMAT" mimeParser query
    wmsCapUpdateSequence <- fromQuery_ query
    return GetCapabilities {..}

  renderRequest GetCapabilities {..} = (query, Nothing)
    where
      query = simpleQueryToQuery $ concat [
          toQueryItems version WMS
        , toQueryItems version Capabilities
        , toQueryItems version wmsCapVersion
        , maybe [] (\fmt -> [("FORMAT", renderMime fmt)]) wmsCapFormat
        , toQueryItems version wmsCapUpdateSequence
        ]
      version = fromMaybe Wms130 wmsCapVersion

instance Common.Request (Request Map) where
  parseRequest (queryCI -> query) _ = do
    wmsMapVersion <- maybe (Left (MissingParameterError "VERSION")) return
                 =<< reqVersion query
    wmsMapLayers <- fromQuery_ query
    wmsMapCrs <- fromQuery wmsMapVersion query
    wmsMapBbox <- fromQuery wmsMapVersion query
    wmsMapSize <- fromQuery_ query
    wmsMapFormat <- mandatoryParameter "FORMAT" mimeParser query
    wmsMapTransparent <- fromQuery_ query
    wmsMapBackground <- fromQuery_ query
    wmsMapExceptions <- fromQuery wmsMapVersion query
    wmsMapTime <- fromQuery_ query
    wmsMapElevation <- optionalParameter "ELEVATION" scientific query
    wmsMapDimensions <- fromQuery_ query
    return GetMap {..}

  renderRequest GetMap {..} = (query, Nothing)
    where
      query = simpleQueryToQuery $ concat [
          toQueryItems wmsMapVersion WMS
        , toQueryItems wmsMapVersion Map
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
        , toQueryItems wmsMapVersion wmsMapDimensions
        ]


instance Common.Request (Request FeatureInfo) where
  parseRequest oQuery@(queryCI -> query) body = do
    wmsInfoMapRequest <- parseRequest oQuery body
    wmsQueryLayers <- namesFromQuery "QUERY_LAYERS" query
    wmsFeatureCount <- optionalParameter "FEATURE_COUNT" positiveInt query
    wmsPixel <- fromQuery (wmsMapVersion wmsInfoMapRequest) query
    wmsInfoFormat <- mandatoryParameter "INFO_FORMAT" mimeParser query
    return GetFeatureInfo {..}

  renderRequest GetFeatureInfo{..} = (query, mapBody)
    where
      (mapQuery, mapBody) = renderRequest wmsInfoMapRequest
      version = wmsMapVersion wmsInfoMapRequest
      query = filter ((/="REQUEST") . fst) mapQuery ++
        simpleQueryToQuery (concat [
          toQueryItems version FeatureInfo
        , [ ("QUERY_LAYERS", renderNames wmsQueryLayers)
          , ("INFO_FORMAT", renderMime wmsInfoFormat)
          ]
        , renderOptionalParameter "FEATURE_COUNT" (fmap show' wmsFeatureCount)
        , toQueryItems version wmsPixel
        ])

reqVersion :: QueryCI -> Either ParseError (Maybe Version)
reqVersion query =
  case fromQuery Wms100 query of
    Right Nothing ->
      case fromQuery Wms111 query of
        Left _ -> fromQuery Wms130 query
        r -> r
    r -> r



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

instance FromQuery Pixel Version where
  fromQuery version query =
    Pixel <$> mandatoryParameter iName (signed decimal) query
          <*> mandatoryParameter jName (signed decimal) query
    where
      iName = case version of {Wms130 -> "I"; _ -> "X"}
      jName = case version of {Wms130 -> "J"; _ -> "Y"}
  {-# INLINE fromQuery #-}

instance ToQueryItems Pixel Version where
  toQueryItems version (Pixel i j) =
    [ (iName, fromString (show i))
    , (jName, fromString (show j))
    ]
    where
      iName = case version of {Wms130 -> "I"; _ -> "X"}
      jName = case version of {Wms130 -> "J"; _ -> "Y"}
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
            stringCI "capabilities" *> pure Capabilities
        <|> stringCI "map"          *> pure Map
        <|> stringCI "feature_info" *> pure FeatureInfo
      wmsRequestParser =
            stringCI "GetCapabilities" *> pure Capabilities
        <|> stringCI "GetMap"          *> pure Map
        <|> stringCI "GetFeatureInfo"  *> pure FeatureInfo
  {-# INLINE fromQuery #-}

instance ToQueryItems RequestType Version where
  toQueryItems Wms100 Capabilities = [("REQUEST", "capabilities")]
  toQueryItems Wms100 Map          = [("REQUEST", "map")]
  toQueryItems Wms100 FeatureInfo  = [("REQUEST", "feature_info")]
  toQueryItems _      Capabilities = [("REQUEST", "GetCapabilities")]
  toQueryItems _      Map          = [("REQUEST", "GetMap")]
  toQueryItems _      FeatureInfo  = [("REQUEST", "GetFeatureInfo")]
  {-# INLINE toQueryItems #-}


--
-- * Crs instances
--

instance FromQuery Crs Version where
  fromQuery Wms130 = mandatoryParameter "CRS" crsParser
  fromQuery _      = mandatoryParameter "SRS" crsParser
  {-# INLINE fromQuery #-}

instance ToQueryItems Crs Version where
  toQueryItems Wms130 crs = [("CRS", renderCrs crs)]
  toQueryItems _      crs = [("SRS", renderCrs crs)]
  {-# INLINE toQueryItems #-}


--
-- * Bbox instances
--


instance FromQuery Bbox Version where
  fromQuery version = mandatoryParameter "BBOX" $ do
    c0 <- scientific <* char ','
    c1 <- scientific <* char ','
    c2 <- scientific <* char ','
    c3 <- scientific
    let (x0,y0,x1,y1) =
          case version of
            Wms130 -> (c1,c0,c3,c2) -- 1.3.0 has lat/lon axis ordering
            _      -> (c0,c1,c2,c3)
    maybe (fail "Invalid bbox") return (mkBbox x0 y0 x1 y1)
  {-# INLINE fromQuery #-}

instance ToQueryItems Bbox Version where
  toQueryItems version Bbox{minx,miny,maxx,maxy} = [ ("BBOX", runBuilder bld)]
    where
      bld = mconcat (L.intersperse "," (map show' coords))
      coords = case version of
        Wms130 -> [miny, minx, maxy, maxx]
        _      -> [minx, miny, maxx, maxy]
  {-# INLINE toQueryItems #-}
