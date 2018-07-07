module Landsat where

import           Control.Lens          ((&), (.~), (^.))
import           Control.Monad         (replicateM)
import           Data.Aeson            (ToJSON, object, toJSON, (.=))
import           Data.ByteString.Char8 (pack)
import           Data.ByteString.Lazy  (ByteString)
import           Data.List             (concat, intersperse)
import           Data.Maybe            (fromMaybe)
import           Data.Semigroup        (Semigroup, (<>))
import qualified Data.Text             as T
import           Data.Word             (Word8)
import           GHC.Generics
import           Network.Wreq          (Options, Response, defaults, header,
                                        param, postWith, responseBody)
import           System.Random         (randomIO)

class Filterable a where
  toFilter :: a -> String

data SensorId =
  TM
  | ETM
  | MSS
  | OLI deriving (Eq, Show)

instance Filterable SensorId where
  toFilter = ("'" ++ ) . (++ "'") . show

data SpacecraftId =
  LANDSAT_4
  | LANDSAT_5
  | LANDSAT_7
  | LANDSAT_8 deriving (Eq, Show)

instance Filterable SpacecraftId where
  toFilter = ("'" ++ ) . (++ "'") . show

data JobPost = JobPost { configuration :: JobConfiguration
                       , jobReference  :: JobReference
                       } deriving (Eq, Show, Generic)

data SortOrder = ASC | DESC deriving (Eq, Show)

instance ToJSON JobPost

data JobReference = JobReference { projectId :: String
                                 , jobId     :: String } deriving (Eq, Show, Generic)

instance ToJSON JobReference

data JobConfiguration = JobConfiguration { queryObj :: LandsatQuery } deriving (Eq, Show)

instance ToJSON JobConfiguration where
  toJSON (JobConfiguration landsatQ) =
    object [ "query" .= toJSON landsatQ ]

data LandsatQuery = LandsatQuery { selectF  :: String
                                 , filters  :: [String]
                                 , ordering :: [String]
                                 , limit    :: Maybe Int
                                 , offset   :: Maybe Int } deriving (Eq, Show)

tableName :: String
tableName = "[bigquery-public-data:cloud_storage_geo_index.landsat_index]"

defaultLandsatQ :: LandsatQuery
defaultLandsatQ = LandsatQuery ("select * from " ++ tableName) [] [] (Just 100) Nothing

-- there's no identity, since the default selectF is always chosen
instance Semigroup LandsatQuery where
  (<>) this that = LandsatQuery { selectF = selectF defaultLandsatQ
                                , filters = filters this <> filters that
                                , ordering = ordering this <> ordering that
                                , limit = min <$> (limit this) <*> (limit that)
                                , offset = max <$> (offset this) <*> (offset that) }

withMinCloudCover :: CloudCover -> LandsatQuery
withMinCloudCover cc =
  defaultLandsatQ
  <> LandsatQuery "" ["cloud_cover >= " ++ show cc] [] Nothing Nothing

withMaxCloudCover :: CloudCover -> LandsatQuery
withMaxCloudCover cc =
  defaultLandsatQ
  <> LandsatQuery "" ["cloud_cover <= " ++ show cc] [] Nothing Nothing

withSensor :: SensorId -> LandsatQuery
withSensor sensor =
  defaultLandsatQ
  <> LandsatQuery "" ["sensor_id = " ++ toFilter sensor] [] Nothing Nothing

withSpacecraft :: SpacecraftId -> LandsatQuery
withSpacecraft spacecraft =
  defaultLandsatQ
  <> LandsatQuery "" ["spacecraft_id = " ++ toFilter spacecraft] [] Nothing Nothing

withLimit :: Int -> LandsatQuery
withLimit limit =
  defaultLandsatQ
  <> LandsatQuery "" [] [] (Just limit) Nothing

withOffset :: Int -> LandsatQuery
withOffset offset =
  defaultLandsatQ
  <> LandsatQuery "" [] [] Nothing (Just offset)

makeFilters :: LandsatQuery -> String
makeFilters (LandsatQuery _ [] _ _ _) = ""
makeFilters (LandsatQuery _ filts _ _ _) =
  (" WHERE " ++) (concat . intersperse " AND " $ filts)

makeOrderings :: LandsatQuery -> String
makeOrderings (LandsatQuery _ _ [] _ _) = ""
makeOrderings (LandsatQuery _ _ orderings _ _) =
  (" ORDER BY " ++) (concat . intersperse ", " $ orderings)

buildQueryString :: LandsatQuery -> String
buildQueryString lq =
  selectF lq
  ++ makeFilters lq
  ++ makeOrderings lq
  ++ fromMaybe "" ((" LIMIT " ++ ) . show <$> limit lq)
  ++ fromMaybe "" ((" OFFSET " ++ ) . show <$> offset lq)

instance ToJSON LandsatQuery where
  toJSON lq =
    object [ "query" .= buildQueryString lq]

type CloudCover = Float

makeJobPost :: LandsatQuery -> String -> JobPost
makeJobPost lq jobId = JobPost (JobConfiguration lq) (JobReference "earth-on-gcs" jobId)

projectName :: String
projectName = "earth-on-gcs"

baseUrl :: String
baseUrl = "https://clients6.google.com/bigquery/v2/projects/" ++ projectName ++ "/jobs"

-- Key should be some kind of access token from service account auth/auth workflow
runQuery :: String -> JobPost -> IO ( Response ByteString )
runQuery key body = postWith opts baseUrl $ toJSON body
  where
    opts = defaults
      & header "Authorization" .~ [pack $ "Bearer " ++ key]
      & param "alt" .~ ["json"]

randomInt :: IO Word8
randomInt = randomIO

randomJobId :: IO String
randomJobId = (\nums -> concat $ show <$> nums) <$> replicateM 4 randomInt


-- TODO: add decoder for bigquery response, see if assuming completion in req/response
-- cycle is reasonable
tryIt :: String -> IO ()
tryIt s = do
  jobId <- randomJobId
  print jobId
  resp <- runQuery s (makeJobPost (defaultLandsatQ <> withLimit 10) jobId)
  print $ (resp ^. responseBody)
