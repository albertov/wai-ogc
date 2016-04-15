{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Network.Wai.Ogc.Common (
    Request          (..)
  , ParseError       (..)
  , Name
  , Service          (..)
  , Bbox
  , Size
  , Time             (..)
  , TimeStamp        (..)
  , Dimension        (..)
  , UpdateSequence   (..)
  , FromQuery        (..)
  , ToQueryItems     (..)
  , QueryCI

  -- * Smart constructors
  , mkSize
  , mkBbox
  , mkName

  -- * Accessors

  -- ** 'Name'
  , unName

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
  , namesFromQuery
  , renderNames
  , fromQuery_
  , mimeParser
  , renderMime
  , renderCrs
  , crsParser
  , queryCI
  , optionalParameter
  , mandatoryParameter
  , renderOptionalParameter
  , renderMandatoryParameter
  , show'
  , runBuilder
  , positiveInt

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
                                         , toLazyByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.ByteString.Lex.Integral (readDecimal)
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
import           SpatialReference as SR
import           Network.HTTP.Types.URI (Query)

--
-- * Public types and functions
--


data ParseError
  = MissingParameterError           ByteString
  | EmptyParameterError             ByteString
  | InvalidParameterError           ByteString ByteString
  | LayersStylesLengthMismatchError
  | NotImplementedError
  deriving (Show, Eq, Typeable)

class Request a where
  parseRequest :: Query -> Maybe ByteString -> Either ParseError a
  -- | Renders a Request into a 'SimpleQuery' and an optional request body
  --   (eg: for WFS POST requests)
  renderRequest :: a -> (Query, Maybe ByteString)


--
-- * Name
--

newtype Name = Name Text
  deriving (Eq, Show, Ord)

instance IsString Name where
  fromString s = fromMaybe (error ("fromString (Name): Invalid Name: " ++ s))
                           (mkName (fromString s))

mkName :: Text -> Maybe Name
mkName name
  | Nothing <- T.findIndex (==',') name = Just (Name name)
  | otherwise                           = Nothing
{-# INLINE mkName #-}

unName :: Name -> Text
unName (Name n) = n
{-# INLINE unName #-}

namesFromQuery :: CI ByteString -> QueryCI -> Either ParseError [Name]
namesFromQuery key query =
  case mandatoryParameter key namesParser query of
    Left (EmptyParameterError _) -> Right [""]
    ns                           -> ns
  where
    namesParser =
      (Name . lenientDecodeUtf8 <$> AP.takeWhile (/=',')) `sepBy1` (char ',')
{-# INLINE namesFromQuery #-}

renderNames :: [Name] -> ByteString
renderNames = BS.intercalate "," . map (T.encodeUtf8 . unName)
{-# INLINE renderNames #-}

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
-- * Size
--

data Size = Size !Int !Int
  deriving (Eq, Show)

mkSize :: Int -> Int -> Maybe Size
mkSize w h
  | w>0 && h>0 = Just (Size w h)
  | otherwise  = Nothing
{-# INLINE CONLIKE mkSize #-}

width, height :: Size -> Int
width  (Size w _) = w
height (Size _ h) = h


instance FromQuery Size () where
  fromQuery () query =
    Size <$> mandatoryParameter "WIDTH" positiveInt query
         <*> mandatoryParameter "HEIGHT" positiveInt query
  {-# INLINE fromQuery #-}

instance ToQueryItems Size c where
  toQueryItems _ (Size w h) =
    [("WIDTH", fromString (show w)) , ("HEIGHT", fromString (show h))]
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
{-# INLINE parseTime #-}

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
  fromQuery :: c -> QueryCI -> Either ParseError a

fromQuery_ :: FromQuery a () => QueryCI -> Either ParseError a
fromQuery_ = fromQuery ()

class ToQueryItems a c  where
  toQueryItems :: c -> a -> [(ByteString, ByteString)]

instance {-# OVERLAPPABLE #-} ToQueryItems a c => ToQueryItems (Maybe a) c where
  toQueryItems c = maybe [] (toQueryItems c)
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


crsParser :: Parser Crs
crsParser = coded "EPSG" <|> coded "CRS" <|> coded "SR-ORG" <|> named
  where
    named = namedCrs <$> (BS.unpack <$> takeByteString)
    coded code = maybe (fail "invalid coded crs") return =<< mCoded code
    mCoded code =
      SR.codedCrs <$> (BS.unpack <$> stringCI code <* char ':')
                  <*> decimal <* endOfInput

renderCrs :: Crs -> ByteString
renderCrs (Coded code val) = runBuilder (fromString code <> ":" <> show' val)
renderCrs (Named name)     = fromString name
renderCrs _                = error "renderCrs: Not implemented"

mimeParser:: Parser MIME.Type
mimeParser = maybe (fail "Invalid MIME") return
          =<< (MIME.parseMIMEType . lenientDecodeUtf8 <$> takeByteString)

renderMime :: MIME.Type -> ByteString
renderMime = T.encodeUtf8 . MIME.showType

--
-- * Utils
--

positiveInt :: Parser Int
positiveInt = do
  n <- decimal
  if n<=0 then fail "Negative number" else return n

lenientDecodeUtf8 :: ByteString -> Text
lenientDecodeUtf8 = T.decodeUtf8With T.lenientDecode
{-# INLINE lenientDecodeUtf8 #-}

mandatoryParameter
  :: CI ByteString
  -> Parser a
  -> QueryCI
  -> Either ParseError a
mandatoryParameter key parser
  = either Left (maybe (Left (MissingParameterError (CI.original key))) Right)
  . optionalParameter key parser
{-# INLINE mandatoryParameter #-}

optionalParameter
  :: CI ByteString
  -> Parser a
  -> QueryCI
  -> Either ParseError (Maybe a)
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
{-# INLINE optionalParameter #-}

renderMandatoryParameter
  :: ByteString -> ByteString -> [(ByteString, ByteString)]
renderMandatoryParameter key = (:[]) . (key,)

renderOptionalParameter
  :: ByteString -> Maybe ByteString -> [(ByteString, ByteString)]
renderOptionalParameter _   Nothing    = []
renderOptionalParameter key (Just val) = [(key, val)]


-- | Parses a query string into case-insensitive elements
queryCI :: Query -> QueryCI
queryCI = map (first CI.mk)
{-# INLINE queryCI #-}

runBuilder :: Builder -> ByteString
runBuilder = LBS.toStrict . toLazyByteString
{-# INLINE runBuilder #-}

show' :: (Show a, IsString b) => a -> b
show' = fromString . show
{-# INLINE show' #-}
