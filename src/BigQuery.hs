module BigQuery where

import           LandsatModel

import           Control.Lens         ((^.), (^?))
import           Control.Monad        (replicateM)
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.ByteString.Lazy (ByteString)
import           Data.Text            (pack)
import           Data.Word            (Word8)
import           GHC.Generics         (Generic)
import           System.Random        (randomIO)

data JobReference = JobReference { projectId :: String
                                 , jobId     :: String } deriving (Eq, Show, Generic)

instance ToJSON JobReference
instance FromJSON JobReference

data QueryCreateResponse =
  QueryCreateResponse { id           :: String
                      , jobReference :: JobReference } deriving (Eq, Show, Generic)

data QueryDetailResponse =
  QueryDetailResponse { rows        :: [Value]
                      , schema      :: Value
                      , jobComplete :: Bool } deriving (Eq, Show, Generic)

instance FromJSON QueryDetailResponse
instance ToJSON QueryDetailResponse

instance FromJSON QueryCreateResponse

randomInt :: IO Word8
randomInt = randomIO

randomJobId :: IO String
randomJobId = (\nums -> concat $ show <$> nums) <$> replicateM 7 randomInt

decodeCreateResponse :: ByteString -> Maybe QueryCreateResponse
decodeCreateResponse = decode

decodeGetResponse :: ByteString -> Maybe QueryDetailResponse
decodeGetResponse = decode
