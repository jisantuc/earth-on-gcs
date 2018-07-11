module Landsat where

import           BigQuery
import           Control.Applicative   ((<|>))
import           Control.Lens          ((&), (.~), (^.))
import           Data.Aeson            (ToJSON, decode, object, toJSON, (.=))
import           Data.ByteString.Char8 (pack)
import           Data.ByteString.Lazy  (ByteString)
import           Data.List             (concat, intersperse)
import           Data.Maybe            (fromMaybe, isJust)
import           Data.Semigroup        (Semigroup, (<>))
import qualified Data.Text             as T
import           Data.Time.Format      (FormatTime, defaultTimeLocale,
                                        formatTime)
import           GHC.Generics
import           LandsatModel
import           Network.Wreq          (Options, Response, defaults, getWith,
                                        header, param, postWith, responseBody)
import           System.Random         (randomIO)
data JobPost = JobPost { configuration :: JobConfiguration
                       , jobReference  :: JobReference
                       } deriving (Eq, Show, Generic)

data SortOrder = ASC | DESC deriving (Eq, Show)

instance ToJSON JobPost

data JobConfiguration = JobConfiguration { queryObj :: LandsatQuery } deriving (Eq, Show)

instance ToJSON JobConfiguration where
  toJSON (JobConfiguration landsatQ) =
    object [ "query" .= toJSON landsatQ ]

data LandsatQuery = LandsatQuery { selectF  :: String
                                 , filters  :: [String]
                                 , ordering :: [String]
                                 , limit    :: Int
                                 , offset   :: Int } deriving (Eq, Show)

tableName :: String
tableName = "[bigquery-public-data:cloud_storage_geo_index.landsat_index]"

defaultLandsatQ :: LandsatQuery
defaultLandsatQ = LandsatQuery ("select * from " ++ tableName) [] [] 100 0

-- there's no identity, since the default selectF is always chosen
instance Semigroup LandsatQuery where
  (<>) this that = LandsatQuery { selectF = selectF defaultLandsatQ
                                , filters = filters this <> filters that
                                , ordering = ordering this <> ordering that
                                , limit = min (limit this) (limit that)
                                , offset = max (offset this) (offset that) }

withMinDateAcquired :: FormatTime t => t -> LandsatQuery
withMinDateAcquired formattable =
  defaultLandsatQ
  <> LandsatQuery "" ["date_acquired >= '" ++ formatTime defaultTimeLocale "%Y-%m-%d" formattable ++ "'"] [] (maxBound :: Int) 0

withMaxDateAcquired :: FormatTime t => t -> LandsatQuery
withMaxDateAcquired formattable =
  defaultLandsatQ
  <> LandsatQuery "" ["date_acquired <= '" ++ formatTime defaultTimeLocale "%Y-%m-%d" formattable ++ "'"] [] (maxBound :: Int) 0

withMinCloudCover :: CloudCover -> LandsatQuery
withMinCloudCover cc =
  defaultLandsatQ
  <> LandsatQuery "" ["cloud_cover >= " ++ show cc] [] (maxBound :: Int) 0

withMaxCloudCover :: CloudCover -> LandsatQuery
withMaxCloudCover cc =
  defaultLandsatQ
  <> LandsatQuery "" ["cloud_cover <= " ++ show cc] [] (maxBound :: Int) 0

withOrdering :: String -> SortOrder -> LandsatQuery
withOrdering fieldName direction =
  defaultLandsatQ
  <> LandsatQuery "" [] [fieldName ++ " " ++ show direction] (maxBound :: Int) 0

withSensor :: SensorId -> LandsatQuery
withSensor sensor =
  defaultLandsatQ
  <> LandsatQuery "" ["sensor_id = " ++ toFilter sensor] [] (maxBound :: Int) 0

withSpacecraft :: SpacecraftId -> LandsatQuery
withSpacecraft spacecraft =
  defaultLandsatQ
  <> LandsatQuery "" ["spacecraft_id = " ++ toFilter spacecraft] [] (maxBound :: Int) 0

withLimit :: Int -> LandsatQuery
withLimit limit =
  defaultLandsatQ
  <> LandsatQuery "" [] [] limit 0

withOffset :: Int -> LandsatQuery
withOffset offset =
  defaultLandsatQ
  <> LandsatQuery "" [] [] (maxBound :: Int) offset

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
  ++ (" LIMIT " ++ ) (show . limit $ lq)
  ++ (" OFFSET " ++ ) (show . offset $ lq)

instance ToJSON LandsatQuery where
  toJSON lq =
    object [ "query" .= buildQueryString lq]

type CloudCover = Float

makeJobPost :: LandsatQuery -> String -> JobPost
makeJobPost lq jobId = JobPost (JobConfiguration lq) (JobReference "earth-on-gcs" jobId)

projectName :: String
projectName = "earth-on-gcs"

baseUrl :: String
baseUrl = "https://clients6.google.com/bigquery/v2/projects/" ++ projectName

jobsBase :: String
jobsBase = baseUrl ++ "/jobs"

queriesBase :: String
queriesBase = baseUrl ++ "/queries"

-- Key should be a bearer token from service account auth/auth workflow
runQuery :: String -> JobPost -> IO ( Response ByteString )
runQuery key body = postWith opts jobsBase $ toJSON body
  where
    opts = defaults
      & header "Authorization" .~ [pack $ "Bearer " ++ key]
      & param "alt" .~ ["json"]

getQuery :: String -> String -> IO (Response ByteString)
getQuery key jobId =
  getWith opts (queriesBase ++ "/" ++ jobId) >>=
    (\resp -> if (checkResp resp) then (return resp) else (getQuery key jobId))
  where
    opts = defaults
      & header "Authorization" .~ [pack $ "Bearer " ++ key]
    checkResp = isJust . decodeGetResponse . (^. responseBody)
