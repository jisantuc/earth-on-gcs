module LandsatModel where

import           Control.Arrow (left)
import           Data.Aeson    (FromJSON, ToJSON)
import           Data.Text     (pack, unpack)
import           GHC.Generics  (Generic)
import           Servant       (FromHttpApiData, parseUrlPiece)
import           Text.Read     (readEither)

class Filterable a where
  toFilter :: a -> String

data SensorId =
  TM
  | ETM
  | MSS
  | OLI_TIRS deriving (Eq, Show, Generic, Read)

instance Filterable SensorId where
  toFilter = ("'" ++ ) . (++ "'") . show
instance FromHttpApiData SensorId where
  parseUrlPiece = left pack . readEither . unpack
instance FromJSON SensorId
instance ToJSON SensorId

data SpacecraftId =
  LANDSAT_4
  | LANDSAT_5
  | LANDSAT_7
  | LANDSAT_8 deriving (Eq, Show, Generic, Read)

instance Filterable SpacecraftId where
  toFilter = ("'" ++ ) . (++ "'") . show
instance FromHttpApiData SpacecraftId where
  parseUrlPiece = left pack . readEither . unpack
instance FromJSON SpacecraftId
instance ToJSON SpacecraftId
