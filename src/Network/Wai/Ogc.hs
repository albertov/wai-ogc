{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Wai.Ogc (
    OgcRequest      (..)
  , OgcRequestError (..)
  , WmsVersion      (..)
  , Service         (..)
  , UpdateSequence
  , parseRequest
  , wmsCapabilitiesRequest
    -- * Re-exports
  , MIME.Type (..)
  , MIME.MIMEType (..)
  , MIME.MIMEParam (..)
) where

import           Control.Arrow ((***))
import qualified Codec.MIME.Parse as MIME
import qualified Codec.MIME.Type as MIME
import           Data.ByteString (ByteString)
import           Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import qualified Data.List as L
import           Data.String (IsString)
import           Data.Text.Encoding (decodeUtf8With)
import           Data.Text.Encoding.Error (lenientDecode)
import           Data.Typeable (Typeable)
import           Network.Wai (Request, queryString)

--
-- * Public types and functions
--

data OgcRequest
  = WcsRequest
  | WfsRequest
  | WmsCapabilitiesRequest {
      format         :: Maybe MIME.Type
    , version        :: Maybe WmsVersion
    , updateSequence :: Maybe UpdateSequence
    }
  | WpsRequest
  deriving (Show)

wmsCapabilitiesRequest :: OgcRequest
wmsCapabilitiesRequest = WmsCapabilitiesRequest Nothing Nothing Nothing

data WmsVersion
  = Wms111
  | Wms130
  deriving (Show, Eq, Ord, Enum, Bounded)

data OgcRequestError
  = UnknownServiceError      ByteString
  | MissingParameterError    ByteString
  | UnknownRequestError      Service ByteString
  | UnknownVersionError      Service ByteString
  | InvalidMimeError         ByteString
  | EmptyUpdateSequenceError
  | NotImplementedError
  deriving (Show, Typeable)


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

type ByteStringCI = CI ByteString

type QueryCI = [(ByteStringCI, Maybe ByteStringCI)]

data Service = WCS | WFS | WMS | WPS
  deriving Show

reqService :: QueryCI -> Either OgcRequestError Service
reqService query =
  case L.lookup "SERVICE" query of
    Just (Just "WCS")   -> Right WCS
    Just (Just "WFS")   -> Right WFS
    Just (Just "WMS")   -> Right WMS
    Just (Just "WPS")   -> Right WPS
    Just (Just service) -> Left (UnknownServiceError (CI.original service))
    Just Nothing        -> Left (UnknownServiceError "")
    Nothing             -> Left (MissingParameterError "SERVICE")

parseWmsRequest :: Request -> Either OgcRequestError OgcRequest
parseWmsRequest req = do
  request <- reqWmsRequest
  case request of
    GetCapabilities ->
      WmsCapabilitiesRequest <$> reqFormat query
                             <*> reqWmsVersion
                             <*> reqUpdateSequence query
    GetMap         -> undefined
    GetFeatureInfo -> undefined
  where
    query = queryStringCI req
    reqWmsRequest =
      case L.lookup "REQUEST" query of
        Just (Just "GetCapabilities") -> Right GetCapabilities
        Just (Just "GetMap")          -> Right GetMap
        Just (Just "GetFeatureInfo")  -> Right GetFeatureInfo
        Just (Just r)                 ->
          Left (UnknownRequestError WMS (CI.original r))
        Just Nothing                  -> Left (UnknownRequestError WMS "")
        Nothing                       -> Left (MissingParameterError "REQUEST")
    reqWmsVersion =
      case L.lookup "VERSION" query of
        Just (Just "1.1.1") -> Right (Just Wms111)
        Just (Just "1.3.0") -> Right (Just Wms130)
        Nothing             -> Right Nothing
        Just (Just r)       -> Left (UnknownVersionError WMS (CI.original r))
        Just Nothing        -> Left (UnknownVersionError WMS (""))

reqFormat :: QueryCI -> Either OgcRequestError (Maybe MIME.Type)
reqFormat query =
  case L.lookup "FORMAT" query of
    Just (Just (CI.original -> fmt))
      | Just mime <- parseMIME fmt -> Right (Just mime)
      | otherwise                  -> Left  (InvalidMimeError fmt)
    Just Nothing                   -> Left  (InvalidMimeError "")
    Nothing                        -> Right Nothing
  where
    parseMIME = MIME.parseMIMEType . decodeUtf8With lenientDecode


reqUpdateSequence :: QueryCI -> Either OgcRequestError (Maybe UpdateSequence)
reqUpdateSequence query =
  case L.lookup "UPDATESEQUENCE" query of
    Just (Just (CI.original -> useq)) -> Right (Just (UpdateSequence useq))
    Just Nothing                      -> Left  EmptyUpdateSequenceError
    Nothing                           -> Right Nothing

-- | Parses a query string into case-insensitive elements
queryStringCI :: Request -> QueryCI
queryStringCI = map (CI.mk *** fmap CI.mk) . queryString
