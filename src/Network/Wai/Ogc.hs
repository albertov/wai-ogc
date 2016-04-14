module Network.Wai.Ogc (
    Request       (..)
  , module Common
) where

import qualified Network.Wai.Ogc.Wms as Wms
import           Network.Wai.Ogc.Common as Common hiding (Request)



data Request
  = Wcs
  | Wfs
  | Wps
  | Wms Wms.Request
  deriving (Show, Eq)

{-
instance ParseRequest Request where
  parseRequest query body = do
    service <- fromQuery_ (queryCI query)
    case service of
      WCS -> Left NotImplementedError
      WFS -> Left NotImplementedError
      WMS -> WmsRequest <$> parseRequest query body
      WPS -> Left NotImplementedError
-}
