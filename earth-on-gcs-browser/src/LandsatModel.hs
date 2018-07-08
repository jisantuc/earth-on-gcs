module LandsatModel where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics (Generic)

class Filterable a where
  toFilter :: a -> String

data SensorId =
  TM
  | ETM
  | MSS
  | OLI deriving (Eq, Show, Generic)

instance Filterable SensorId where
  toFilter = ("'" ++ ) . (++ "'") . show
instance FromJSON SensorId
instance ToJSON SensorId

data SpacecraftId =
  LANDSAT_4
  | LANDSAT_5
  | LANDSAT_7
  | LANDSAT_8 deriving (Eq, Show, Generic)

instance Filterable SpacecraftId where
  toFilter = ("'" ++ ) . (++ "'") . show
instance FromJSON SpacecraftId
instance ToJSON SpacecraftId
