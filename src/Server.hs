module Server where

import           BigQuery
import           Landsat
import           LandsatModel

import           Control.Lens               ((^.))
import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson                 (decode)
import           Data.ByteString.Lazy       (ByteString)
import           Data.ByteString.Lazy.Char8 (pack)
import           Data.Maybe                 (fromMaybe)
import           Data.Semigroup             ((<>))
import           Data.Time                  (Day)
import           GHC.Generics
import           Network.Wreq               (responseBody)
import           Servant
import           Servant.API.Capture
import           System.Environment         (getEnv)

type LandsatSceneApi = "landsat"
  :> QueryParam "cloudCoverMin" Float
  :> QueryParam "cloudCoverMax" Float
  :> QueryParam "dateAcquiredMin" String
  :> QueryParam "dateAcquiredMax" String
  :> QueryParam "spacecraftId" SpacecraftId
  :> QueryParam "sensorId" SensorId
  :> QueryParam "page" Int
  :> QueryParam "limit" Int
  :> Get '[JSON] (Maybe QueryDetailResponse)

landsatHandler :: Maybe Float
  -> Maybe Float
  -> Maybe String
  -> Maybe String
  -> Maybe SpacecraftId
  -> Maybe SensorId
  -> Maybe Int
  -> Maybe Int
  -> Handler (Maybe QueryDetailResponse)
landsatHandler minCC maxCC minDate maxDate spacecraft sensor page limit = do
  jobId <- liftIO $ randomJobId
  liftIO . print $ " Query for job id: " ++ jobId ++ ": " ++ buildQueryString landsatQuery
  gcpKey <- liftIO $ getEnv "GCP_AUTH_KEY"
  resp <- liftIO $ runQuery gcpKey (makeJobPost landsatQuery jobId)
  getResp <- liftIO $ getQuery gcpKey jobId
  return $ decodeGetResponse (getResp ^. responseBody)
  where
    landsatQuery =
      withDefault (withMinCloudCover <$> minCC)
      <> withDefault (withMaxCloudCover <$> maxCC)
      <> withDefault (withMinDateAcquired <$> decodeDate minDate)
      <> withDefault (withMaxDateAcquired <$> decodeDate maxDate)
      <> withDefault (withSpacecraft <$> spacecraft)
      <> withDefault (withSensor <$> sensor)
      <> withDefault (withLimit <$> limit)
      <> withDefault (withOffset <$> ((*) <$> limit <*> page))
    withDefault = (fromMaybe defaultLandsatQ)
    decodeDate d = (pack . ("\"" ++ ) . (++ "\"") <$> d) >>= (decode :: ByteString -> Maybe Day)

landsatServer :: Server LandsatSceneApi
landsatServer = landsatHandler

landsatAPI :: Proxy LandsatSceneApi
landsatAPI = Proxy

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
application :: Application
application = serve landsatAPI landsatServer
